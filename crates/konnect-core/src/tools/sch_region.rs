//! Connectivity-preserving movement of complete schematic regions.
//!
//! This is intentionally independent of the cosmetic graphics API.  It edits
//! top-level native S-expressions by block, so UUIDs and symbol references are
//! left byte-for-byte intact while every member of a coherent block gets the
//! same translation.

use crate::mcp::protocol::CallToolResult;
use crate::tools::{get_path, require_f64, ToolContext};
use konnect_sexp::parser::SexpNode;
use konnect_sexp::writer::{
    apply_edits, find_balanced_block, find_block_starts, find_direct_child_blocks, read_consistent,
    write_atomic_if_unchanged, SexpEdit,
};
use serde_json::{json, Map, Value};
use std::io::Write;

#[derive(Clone, Copy)]
pub struct Box2 {
    pub x1: f64,
    pub y1: f64,
    pub x2: f64,
    pub y2: f64,
}

impl Box2 {
    fn contains(self, x: f64, y: f64) -> bool {
        x >= self.x1 && x <= self.x2 && y >= self.y1 && y <= self.y2
    }
}

fn finite(value: f64, name: &str) -> anyhow::Result<f64> {
    if !value.is_finite() || value.abs() > 1_000_000.0 {
        anyhow::bail!("{name} must be finite and within ±1000000 mm");
    }
    Ok(value)
}

fn uuid(node: &SexpNode) -> Option<String> {
    node.find("uuid")?.get(1)?.as_str().map(ToOwned::to_owned)
}

fn xy(node: &SexpNode, tag: &str) -> Option<(f64, f64)> {
    let n = node.find(tag)?;
    Some((n.get_f64(1)?, n.get_f64(2)?))
}

fn at(node: &SexpNode) -> Option<(f64, f64)> {
    xy(node, "at")
}

fn coord_span(block: &str, tag: &str) -> Option<(usize, usize)> {
    let start = *find_block_starts(block, tag).first()?;
    find_balanced_block(block, start)
}

fn coord(block: &str, tag: &str) -> Option<(f64, f64, Option<f64>)> {
    let (start, end) = coord_span(block, tag)?;
    let n = konnect_sexp::parse_sexp(&block[start..end]).ok()?;
    Some((n.get_f64(1)?, n.get_f64(2)?, n.get_f64(3)))
}

fn fmt(value: f64) -> String {
    let mut result = format!("{value:.6}");
    while result.contains('.') && result.ends_with('0') {
        result.pop();
    }
    if result.ends_with('.') {
        result.pop();
    }
    if result == "-0" {
        "0".into()
    } else {
        result
    }
}

fn translated_coord(tag: &str, x: f64, y: f64, rotation: Option<f64>) -> String {
    match rotation {
        Some(r) => format!("({tag} {} {} {})", fmt(x), fmt(y), fmt(r)),
        None => format!("({tag} {} {})", fmt(x), fmt(y)),
    }
}

fn translate_coord(
    block: &str,
    tag: &str,
    dx: f64,
    dy: f64,
    edits: &mut Vec<SexpEdit>,
) -> anyhow::Result<()> {
    let Some((start, end)) = coord_span(block, tag) else {
        return Ok(());
    };
    let (x, y, rotation) =
        coord(block, tag).ok_or_else(|| anyhow::anyhow!("invalid ({tag}) coordinate"))?;
    edits.push(SexpEdit::replace(
        start,
        end,
        translated_coord(
            tag,
            x + dx,
            y + dy,
            (tag == "at").then_some(rotation).flatten(),
        ),
    ));
    Ok(())
}

fn translate_block(block: &str, kind: &str, dx: f64, dy: f64) -> anyhow::Result<String> {
    let mut edits = Vec::new();
    match kind {
        "wire" => {
            for tag in ["xy", "start", "end"] {
                // `xy` is handled below as a pair; legacy start/end is direct.
                if tag != "xy" {
                    translate_coord(block, tag, dx, dy, &mut edits)?;
                }
            }
            let starts = find_block_starts(block, "xy");
            for start in starts {
                let end = find_balanced_block(block, start)
                    .ok_or_else(|| anyhow::anyhow!("invalid wire point"))?
                    .1;
                let n = konnect_sexp::parse_sexp(&block[start..end])
                    .ok()
                    .ok_or_else(|| anyhow::anyhow!("invalid wire point"))?;
                let x = n
                    .get_f64(1)
                    .ok_or_else(|| anyhow::anyhow!("invalid wire x"))?;
                let y = n
                    .get_f64(2)
                    .ok_or_else(|| anyhow::anyhow!("invalid wire y"))?;
                edits.push(SexpEdit::replace(
                    start,
                    end,
                    translated_coord("xy", x + dx, y + dy, None),
                ));
            }
        }
        "symbol" => {
            translate_coord(block, "at", dx, dy, &mut edits)?;
            // Placed symbol field anchors are absolute sheet coordinates. Move
            // each direct property anchor with its symbol, but do not touch
            // embedded library geometry or instance-path metadata.
            for property_start in find_block_starts(block, "property") {
                let property_end = find_balanced_block(block, property_start)
                    .ok_or_else(|| anyhow::anyhow!("invalid symbol property"))?
                    .1;
                let property = &block[property_start..property_end];
                if let Some((at_start, at_end)) = coord_span(property, "at") {
                    let (x, y, rotation) = coord(property, "at")
                        .ok_or_else(|| anyhow::anyhow!("invalid property anchor"))?;
                    edits.push(SexpEdit::replace(
                        property_start + at_start,
                        property_start + at_end,
                        translated_coord("at", x + dx, y + dy, rotation),
                    ));
                }
            }
        }
        "label" | "global_label" | "hierarchical_label" | "junction" | "no_connect" | "text"
        | "text_box" => {
            translate_coord(block, "at", dx, dy, &mut edits)?;
        }
        "rectangle" => {
            translate_coord(block, "start", dx, dy, &mut edits)?;
            translate_coord(block, "end", dx, dy, &mut edits)?;
        }
        _ => anyhow::bail!("unsupported move_region item kind '{kind}'"),
    }
    Ok(apply_edits(block.to_owned(), edits))
}

fn wire_points(node: &SexpNode) -> Option<((f64, f64), (f64, f64))> {
    if let Some(pts) = node.find("pts") {
        let points = pts.find_all("xy");
        if points.len() >= 2 {
            return Some((
                (points[0].get_f64(1)?, points[0].get_f64(2)?),
                (points[1].get_f64(1)?, points[1].get_f64(2)?),
            ));
        }
    }
    Some((xy(node, "start")?, xy(node, "end")?))
}

fn segment_hits_box(a: (f64, f64), b: (f64, f64), area: Box2) -> bool {
    if area.contains(a.0, a.1) || area.contains(b.0, b.1) {
        return true;
    }
    let dx = b.0 - a.0;
    let dy = b.1 - a.1;
    let mut lo: f64 = 0.0;
    let mut hi: f64 = 1.0;
    for (p, q) in [
        (-dx, a.0 - area.x1),
        (dx, area.x2 - a.0),
        (-dy, a.1 - area.y1),
        (dy, area.y2 - a.1),
    ] {
        if p.abs() < f64::EPSILON {
            if q < 0.0 {
                return false;
            }
        } else {
            let t = q / p;
            if p < 0.0 {
                lo = lo.max(t);
            } else {
                hi = hi.min(t);
            }
            if lo > hi {
                return false;
            }
        }
    }
    true
}

fn kind(node: &SexpNode) -> Option<&'static str> {
    match node.head()? {
        "wire" => Some("wire"),
        "label" => Some("label"),
        "global_label" => Some("global_label"),
        "hierarchical_label" => Some("hierarchical_label"),
        "junction" => Some("junction"),
        "no_connect" => Some("no_connect"),
        "rectangle" => Some("rectangle"),
        "text" => Some("text"),
        "text_box" => Some("text_box"),
        "symbol" if node.find("lib_id").is_some() => {
            let lib = node.find_str("lib_id").unwrap_or("");
            if lib.starts_with("power:") {
                Some("power_symbol")
            } else {
                Some("symbol")
            }
        }
        _ => None,
    }
}

fn inc(counts: &mut Map<String, Value>, key: &str) {
    let value = counts.get(key).and_then(Value::as_u64).unwrap_or(0) + 1;
    counts.insert(key.to_owned(), json!(value));
}

/// Move the complete top-level members of a sheet and return the new source
/// plus a structured summary. This pure function is also the regression-test
/// seam for the MCP handler.
pub fn move_region_content(
    content: &str,
    area: Box2,
    dx: f64,
    dy: f64,
) -> anyhow::Result<(String, Value)> {
    let mut edits = Vec::new();
    let mut counts = Map::new();
    let mut moved = Vec::new();
    let mut skipped = Vec::new();

    for (start, end) in find_direct_child_blocks(content, "kicad_sch") {
        let block = &content[start..end];
        let Some(node) = konnect_sexp::parse_sexp(block).ok() else {
            continue;
        };
        let Some(kind) = kind(&node) else {
            continue;
        };
        let Some(id) = uuid(&node) else {
            continue;
        };
        let decision = match kind {
            "wire" => {
                let Some((a, b)) = wire_points(&node) else {
                    continue;
                };
                if area.contains(a.0, a.1) && area.contains(b.0, b.1) {
                    Some(true)
                } else if segment_hits_box(a, b, area) {
                    skipped.push(json!({"kind":kind,"uuid":id,"reason":"boundary_crossing_wire"}));
                    Some(false)
                } else {
                    None
                }
            }
            "rectangle" => {
                let a = xy(&node, "start");
                let b = xy(&node, "end");
                match (a, b) {
                    (Some(a), Some(b))
                        if area.contains(a.0.min(b.0), a.1.min(b.1))
                            && area.contains(a.0.max(b.0), a.1.max(b.1)) =>
                    {
                        Some(true)
                    }
                    (Some(a), Some(b))
                        if segment_hits_box(a, b, area)
                            || area.contains(a.0, a.1)
                            || area.contains(b.0, b.1) =>
                    {
                        skipped.push(
                            json!({"kind":kind,"uuid":id,"reason":"boundary_crossing_object"}),
                        );
                        Some(false)
                    }
                    _ => None,
                }
            }
            _ => at(&node)
                .filter(|(x, y)| area.contains(*x, *y))
                .map(|_| true),
        };
        if decision == Some(true) {
            let replacement = translate_block(
                block,
                if kind == "power_symbol" {
                    "symbol"
                } else {
                    kind
                },
                dx,
                dy,
            )?;
            edits.push(SexpEdit::replace(start, end, replacement));
            inc(&mut counts, kind);
            moved.push(json!({"kind":kind,"uuid":id}));
        }
    }

    let new_content = apply_edits(content.to_owned(), edits);
    Ok((
        new_content,
        json!({"moved_counts":counts,"moved":moved,"skipped":skipped,"skipped_count":skipped.len()}),
    ))
}

/// Validate a candidate in an isolated temporary file before it can replace
/// the user's schematic. Structural parsing catches malformed S-expressions;
/// KiCad's own PDF export is the loadability gate that catches native-schema
/// defects such as an invalid coordinate arity. The temporary files are
/// removed when this function returns.
async fn validate_candidate(cli: &str, content: &str) -> Value {
    let mut evidence = json!({
        "structural_parse": { "ok": false },
        "kicad_cli_export_pdf": { "ok": false },
        "valid": false,
    });

    let root = match konnect_sexp::parse_sexp(content) {
        Ok(root) => root,
        Err(error) => {
            evidence["structural_parse"] = json!({
                "ok": false,
                "error": error.to_string(),
            });
            return evidence;
        }
    };
    if root.head() != Some("kicad_sch") {
        evidence["structural_parse"] = json!({
            "ok": false,
            "error": "candidate root is not kicad_sch",
        });
        return evidence;
    }
    evidence["structural_parse"] = json!({ "ok": true });

    let directory = match tempfile::tempdir() {
        Ok(directory) => directory,
        Err(error) => {
            evidence["kicad_cli_export_pdf"] = json!({
                "ok": false,
                "error": format!("cannot create validation directory: {error}"),
            });
            return evidence;
        }
    };
    let staged = directory.path().join("move-region-candidate.kicad_sch");
    let pdf = directory.path().join("move-region-candidate.pdf");
    let write_result = (|| -> anyhow::Result<()> {
        let mut file = std::fs::File::create(&staged)?;
        file.write_all(content.as_bytes())?;
        file.flush()?;
        Ok(())
    })();
    if let Err(error) = write_result {
        evidence["kicad_cli_export_pdf"] = json!({
            "ok": false,
            "error": format!("cannot stage candidate: {error}"),
        });
        return evidence;
    }

    match crate::tools::cli::export_schematic_pdf(cli, &staged, &pdf).await {
        Ok(()) => {
            evidence["kicad_cli_export_pdf"] = json!({
                "ok": true,
                "output_bytes": std::fs::metadata(&pdf).map(|metadata| metadata.len()).unwrap_or(0),
            });
            evidence["valid"] = json!(true);
        }
        Err(error) => {
            evidence["kicad_cli_export_pdf"] = json!({
                "ok": false,
                "error": error.to_string(),
            });
        }
    }
    evidence
}

pub async fn handle_move_region(
    args: &Value,
    _ctx: &ToolContext,
) -> anyhow::Result<CallToolResult> {
    let path = get_path(args, "schematic")?;
    let values = ["x1", "y1", "x2", "y2", "dx", "dy"];
    let mut parsed = [0.0; 6];
    for (slot, name) in parsed.iter_mut().zip(values) {
        *slot = match require_f64(args, name) {
            Ok(value) => finite(value, name)?,
            Err(error) => return Ok(error),
        };
    }
    if parsed[0] >= parsed[2] || parsed[1] >= parsed[3] {
        anyhow::bail!("region bounds must satisfy x1 < x2 and y1 < y2");
    }
    let area = Box2 {
        x1: parsed[0],
        y1: parsed[1],
        x2: parsed[2],
        y2: parsed[3],
    };
    let old = read_consistent(&path)?;
    let (new_content, summary) = move_region_content(&old, area, parsed[4], parsed[5])?;
    let validation = validate_candidate(&_ctx.config.kicad_cli, &new_content).await;
    if !validation["valid"].as_bool().unwrap_or(false) {
        let evidence = serde_json::to_string(&validation)?;
        return Ok(CallToolResult::error_kind(
            crate::mcp::error::ToolErrorKind::HandlerError {
                reason: format!("move_region validation failed; evidence={evidence}"),
            },
            "move_region refused to replace the schematic; the original was preserved byte-for-byte",
        ));
    }

    if let Err(error) = write_atomic_if_unchanged(&path, &old, &new_content) {
        let evidence = serde_json::to_string(&validation)?;
        return Ok(CallToolResult::error_kind(
            crate::mcp::error::ToolErrorKind::HandlerError {
                reason: format!("move_region commit failed; evidence={evidence}; error={error}"),
            },
            "move_region could not commit the validated candidate; the source was not replaced",
        ));
    }

    let mut result = summary;
    if let Some(object) = result.as_object_mut() {
        object.insert("validation".to_owned(), validation);
    }
    Ok(CallToolResult::json(&result))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mcp::protocol::ToolContent;
    use crate::router::ToolRouter;
    use crate::tools::{ServerConfig, ToolContext};
    use std::sync::Arc;

    const FIXTURE: &str = include_str!("../../tests/fixtures/move_region_native.kicad_sch");

    fn args(path: &std::path::Path, x1: f64, y1: f64, x2: f64, y2: f64, dx: f64, dy: f64) -> Value {
        json!({
            "schematic": path.to_string_lossy(),
            "x1": x1,
            "y1": y1,
            "x2": x2,
            "y2": y2,
            "dx": dx,
            "dy": dy,
        })
    }

    fn result_body(result: &CallToolResult) -> Value {
        let ToolContent::Text { text } = &result.content[0] else {
            panic!("move_region result was not text")
        };
        serde_json::from_str(text).expect("move_region result was not JSON")
    }

    fn test_context(kicad_cli: &str) -> ToolContext {
        ToolContext::new(
            ServerConfig {
                kicad_cli: kicad_cli.to_owned(),
                ..ServerConfig::default()
            },
            Arc::new(ToolRouter::new()),
        )
    }

    #[test]
    fn moves_complete_block_and_reports_boundary_wire() {
        let area = Box2 {
            x1: 10.0,
            y1: 10.0,
            x2: 50.0,
            y2: 50.0,
        };
        let (out, summary) = move_region_content(FIXTURE, area, 5.0, 7.0).unwrap();
        assert!(out.contains("(xy 15 17)") && out.contains("(xy 25 17)"));
        assert!(out.contains("(at 20 27 0)"));
        assert!(out.contains("(at 35 47 0)"));
        assert!(out.contains("(junction (at 25 17)"));
        assert!(out.contains("(no_connect (at 27 29)"));
        assert!(!out.contains("(junction (at 25 17 0)"));
        assert!(!out.contains("(no_connect (at 27 29 0)"));
        assert!(out.contains("(start 15 17)") && out.contains("(end 45 47)"));
        assert!(out.contains("(uuid \"internal-wire\")"));
        assert!(out.contains("(uuid \"power-1\")"));
        assert!(out.contains("(uuid \"rect-1\")"));
        assert!(
            out.contains("(xy 5 25) (xy 60 25)"),
            "crossing wire changed: {out}"
        );
        assert_eq!(summary["moved_counts"]["wire"], 1);
        assert_eq!(summary["moved_counts"]["label"], 1);
        assert_eq!(summary["moved_counts"]["power_symbol"], 1);
        assert_eq!(summary["moved_counts"]["rectangle"], 1);
        assert_eq!(summary["moved_counts"]["text"], 1);
        assert_eq!(summary["moved_counts"]["symbol"], 1);
        assert_eq!(summary["skipped_count"], 1);
        assert_eq!(summary["skipped"][0]["uuid"], "boundary-wire");
    }

    #[tokio::test]
    async fn handler_round_trips_native_fixture_through_kicad_and_preserves_ids() {
        let directory = tempfile::tempdir().unwrap();
        let schematic = directory.path().join("move_region_native.kicad_sch");
        std::fs::write(&schematic, FIXTURE).unwrap();
        let context = test_context("kicad-cli");

        let moved = handle_move_region(
            &args(&schematic, 10.0, 10.0, 50.0, 50.0, 5.0, 7.0),
            &context,
        )
        .await
        .unwrap();
        assert!(!moved.is_error, "move_region failed: {moved:?}");
        let moved_body = result_body(&moved);
        assert_eq!(moved_body["validation"]["valid"], true);
        assert_eq!(moved_body["moved_counts"]["junction"], 1);
        assert_eq!(moved_body["moved_counts"]["no_connect"], 1);
        assert_eq!(moved_body["moved_counts"]["power_symbol"], 1);
        assert_eq!(moved_body["moved_counts"]["rectangle"], 1);
        assert_eq!(moved_body["moved_counts"]["text"], 1);
        assert_eq!(moved_body["skipped"][0]["uuid"], "boundary-wire");

        let after_move = std::fs::read_to_string(&schematic).unwrap();
        for id in [
            "internal-wire",
            "boundary-wire",
            "label-1",
            "junction-1",
            "no-connect-1",
            "symbol-1",
            "power-1",
            "rect-1",
            "text-1",
        ] {
            assert_eq!(
                after_move.matches(&format!("(uuid \"{id}\")")).count(),
                1,
                "UUID {id} changed"
            );
        }
        assert!(after_move.contains("(xy 5 25) (xy 60 25)"));
        assert!(after_move.contains("(label \"SIG\""));
        assert!(after_move.contains("(lib_id \"power:GND\")"));

        let inverse = handle_move_region(
            &args(&schematic, 15.0, 17.0, 55.0, 57.0, -5.0, -7.0),
            &context,
        )
        .await
        .unwrap();
        assert!(!inverse.is_error, "inverse move_region failed: {inverse:?}");
        assert_eq!(result_body(&inverse)["validation"]["valid"], true);
        assert_eq!(std::fs::read_to_string(&schematic).unwrap(), FIXTURE);
    }

    #[tokio::test]
    async fn validation_failure_preserves_original_bytes_and_reports_evidence() {
        let directory = tempfile::tempdir().unwrap();
        let schematic = directory.path().join("move_region_native.kicad_sch");
        std::fs::write(&schematic, FIXTURE).unwrap();
        let before = std::fs::read(&schematic).unwrap();
        let context = test_context("konnect-cli-that-does-not-exist");

        let result = handle_move_region(
            &args(&schematic, 10.0, 10.0, 50.0, 50.0, 5.0, 7.0),
            &context,
        )
        .await
        .unwrap();

        assert!(result.is_error);
        let body = result_body(&result);
        assert_eq!(body["error"]["kind"], "handler_error");
        let reason = body["error"]["reason"].as_str().unwrap();
        assert!(reason.contains("move_region validation failed"));
        assert!(reason.contains("kicad_cli_export_pdf"));
        assert_eq!(std::fs::read(&schematic).unwrap(), before);
    }
}
