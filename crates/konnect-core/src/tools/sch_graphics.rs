//! Stable-UUID management for native schematic graphics and annotations.
//!
//! KiCad keeps sheet-level rectangles, text, and text boxes as native
//! top-level S-expressions.  They are deliberately edited as targeted raw
//! blocks so their UUIDs, formatting, and style children survive unchanged.

use crate::mcp::protocol::CallToolResult;
use crate::tool;
use crate::tools::{get_path, require_f64, require_str, ToolContext, ToolDef};
use konnect_sexp::parser::SexpNode;
use konnect_sexp::writer::{
    apply_edits, find_balanced_block, find_block_starts, find_direct_child_blocks, read_consistent,
    write_atomic_if_unchanged, SexpEdit,
};
use serde_json::{json, Value};
use std::path::Path;

const GRAPHIC_KINDS: &[&str] = &["rectangle", "text", "text_box"];

#[derive(Debug, Clone)]
struct GraphicRecord {
    start: usize,
    end: usize,
    kind: String,
    uuid: String,
    x: Option<f64>,
    y: Option<f64>,
    rotation: f64,
    start_xy: Option<(f64, f64)>,
    end_xy: Option<(f64, f64)>,
    text: Option<String>,
    style_tags: Vec<String>,
}

pub fn tools() -> Vec<ToolDef> {
    vec![
        tool!(
            "list_schematic_graphics",
            "List native sheet graphics and annotations (rectangles, text, and text boxes) with stable UUIDs, geometry, rotation, text, and style data.",
            json!({"type":"object","properties":{"schematic":{"type":"string"}},"required":["schematic"]}),
            |args, ctx| async move { handle_list(args, ctx).await }
        ),
        tool!(
            "move_schematic_graphic",
            "Move one native schematic graphic or annotation by stable UUID. The UUID and all style data are preserved.",
            json!({"type":"object","properties":{"schematic":{"type":"string"},"uuid":{"type":"string"},"dx":{"type":"number"},"dy":{"type":"number"}},"required":["schematic","uuid","dx","dy"]}),
            |args, ctx| async move { handle_move(args, ctx).await }
        ),
        tool!(
            "edit_schematic_graphic",
            "Edit a native schematic graphic or annotation by stable UUID. Text, anchor position, and rotation are supported; omitted fields are unchanged.",
            json!({"type":"object","properties":{"schematic":{"type":"string"},"uuid":{"type":"string"},"text":{"type":"string"},"x":{"type":"number"},"y":{"type":"number"},"rotation":{"type":"number"}},"required":["schematic","uuid"]}),
            |args, ctx| async move { handle_edit(args, ctx).await }
        ),
        tool!(
            "delete_schematic_graphic",
            "Delete one native schematic graphic or annotation by stable UUID without touching other sheet items.",
            json!({"type":"object","properties":{"schematic":{"type":"string"},"uuid":{"type":"string"}},"required":["schematic","uuid"]}),
            |args, ctx| async move { handle_delete(args, ctx).await }
        ),
    ]
}

fn valid_number(v: f64, name: &str) -> anyhow::Result<f64> {
    if !v.is_finite() || v.abs() > 1_000_000.0 {
        anyhow::bail!("{} must be finite and within ±1000000 mm", name);
    }
    Ok(v)
}

fn node_uuid(node: &SexpNode) -> Option<String> {
    node.find("uuid")?.get(1)?.as_str().map(ToOwned::to_owned)
}

fn node_xy(node: &SexpNode, tag: &str) -> Option<(f64, f64)> {
    let child = node.find(tag)?;
    Some((child.get_f64(1)?, child.get_f64(2)?))
}

fn node_at(node: &SexpNode) -> Option<(f64, f64, f64)> {
    let at = node.find("at")?;
    Some((at.get_f64(1)?, at.get_f64(2)?, at.get_f64(3).unwrap_or(0.0)))
}

fn style_tags(node: &SexpNode) -> Vec<String> {
    node.children()
        .unwrap_or(&[])
        .iter()
        .filter_map(|child| child.head())
        .filter(|tag| matches!(*tag, "stroke" | "fill" | "effects" | "justify" | "margins"))
        .map(ToOwned::to_owned)
        .collect()
}

fn records(content: &str) -> Vec<GraphicRecord> {
    find_direct_child_blocks(content, "kicad_sch")
        .into_iter()
        .filter_map(|(start, end)| {
            let block = &content[start..end];
            let node = konnect_sexp::parse_sexp(block).ok()?;
            let kind = node.head()?.to_string();
            if !GRAPHIC_KINDS.contains(&kind.as_str()) {
                return None;
            }
            let uuid = node_uuid(&node)?;
            let (x, y, rotation) = node_at(&node)
                .map(|(x, y, r)| (Some(x), Some(y), r))
                .unwrap_or((None, None, 0.0));
            let start_xy = node_xy(&node, "start");
            let end_xy = node_xy(&node, "end");
            if kind == "rectangle" && (start_xy.is_none() || end_xy.is_none()) {
                return None;
            }
            let text = if matches!(kind.as_str(), "text" | "text_box") {
                node.get(1)
                    .and_then(SexpNode::as_str)
                    .map(ToOwned::to_owned)
            } else {
                None
            };
            Some(GraphicRecord {
                start,
                end,
                kind,
                uuid,
                x,
                y,
                rotation,
                start_xy,
                end_xy,
                text,
                style_tags: style_tags(&node),
            })
        })
        .collect()
}

fn record<'a>(content: &str, uuid: &str) -> anyhow::Result<GraphicRecord> {
    records(content)
        .into_iter()
        .find(|item| item.uuid == uuid)
        .ok_or_else(|| {
            anyhow::anyhow!(
                "Schematic graphic UUID '{}' not found on the current sheet",
                uuid
            )
        })
}

fn style_value(record: &GraphicRecord, block: &str) -> Value {
    json!({
        "tags": record.style_tags,
        "raw": block,
    })
}

fn output_item(record: &GraphicRecord, content: &str) -> Value {
    let mut item = json!({
        "uuid": record.uuid,
        "type": record.kind,
        "x": record.x,
        "y": record.y,
        "rotation": record.rotation,
        "text": record.text,
        "style": style_value(record, &content[record.start..record.end]),
    });
    if let (Some((sx, sy)), Some((ex, ey))) = (record.start_xy, record.end_xy) {
        item["geometry"] = json!({"start":{"x":sx,"y":sy},"end":{"x":ex,"y":ey},"width":(ex-sx).abs(),"height":(ey-sy).abs()});
    } else {
        item["geometry"] = json!({"position":{"x":record.x,"y":record.y}});
    }
    item
}

fn coord_span(block: &str, tag: &str) -> Option<(usize, usize)> {
    let start = *find_block_starts(block, tag).first()?;
    find_balanced_block(block, start)
}

fn coord_from_block(block: &str, tag: &str) -> Option<(f64, f64, f64)> {
    let (start, end) = coord_span(block, tag)?;
    let node = konnect_sexp::parse_sexp(&block[start..end]).ok()?;
    Some((
        node.get_f64(1)?,
        node.get_f64(2)?,
        node.get_f64(3).unwrap_or(0.0),
    ))
}

fn coord_text(tag: &str, x: f64, y: f64, rotation: Option<f64>) -> String {
    match rotation {
        Some(r) => format!("({tag} {} {} {})", fmt(x), fmt(y), fmt(r)),
        None => format!("({tag} {} {})", fmt(x), fmt(y)),
    }
}

fn fmt(v: f64) -> String {
    let mut s = format!("{v:.6}");
    while s.contains('.') && s.ends_with('0') {
        s.pop();
    }
    if s.ends_with('.') {
        s.pop();
    }
    if s == "-0" {
        "0".into()
    } else {
        s
    }
}

fn translate_block(block: &str, kind: &str, dx: f64, dy: f64) -> anyhow::Result<String> {
    let mut edits = Vec::new();
    let move_coord = |tag: &str, edits: &mut Vec<SexpEdit>| -> anyhow::Result<()> {
        if let Some((start, end)) = coord_span(block, tag) {
            let (x, y, r) = coord_from_block(block, tag)
                .ok_or_else(|| anyhow::anyhow!("invalid ({tag}) geometry"))?;
            edits.push(SexpEdit::replace(
                start,
                end,
                coord_text(
                    tag,
                    valid_number(x + dx, "x")?,
                    valid_number(y + dy, "y")?,
                    (tag == "at").then_some(r),
                ),
            ));
        }
        Ok(())
    };
    match kind {
        "rectangle" => {
            move_coord("start", &mut edits)?;
            move_coord("end", &mut edits)?;
        }
        "text" | "text_box" => {
            if coord_span(block, "at").is_some() {
                move_coord("at", &mut edits)?;
            } else {
                move_coord("start", &mut edits)?;
                move_coord("end", &mut edits)?;
            }
        }
        _ => anyhow::bail!("unsupported schematic graphic type '{kind}'"),
    }
    Ok(apply_edits(block.to_owned(), edits))
}

fn first_string_span(block: &str) -> Option<(usize, usize)> {
    let head_end = block.find(' ')?;
    let bytes = block.as_bytes();
    let mut i = head_end;
    while i < bytes.len() && bytes[i] != b'"' {
        i += 1;
    }
    if i == bytes.len() {
        return None;
    }
    let start = i;
    i += 1;
    let mut escaped = false;
    while i < bytes.len() {
        if escaped {
            escaped = false;
        } else if bytes[i] == b'\\' {
            escaped = true;
        } else if bytes[i] == b'"' {
            return Some((start, i + 1));
        }
        i += 1;
    }
    None
}

fn quote(s: &str) -> String {
    format!(
        "\"{}\"",
        s.replace('\\', "\\\\")
            .replace('"', "\\\"")
            .replace('\n', "\\n")
    )
}

fn edit_content(content: &str, uuid: &str, args: &Value) -> anyhow::Result<String> {
    let item = record(content, uuid)?;
    let block = &content[item.start..item.end];
    let mut edited = block.to_owned();
    if let Some(text) = args.get("text").and_then(Value::as_str) {
        if let Some((s, e)) = first_string_span(&edited) {
            edited.replace_range(s..e, &quote(text));
        } else {
            anyhow::bail!("graphic '{}' has no editable text", uuid);
        }
    }
    let has_xy = args.get("x").is_some() || args.get("y").is_some();
    if has_xy {
        let x = args
            .get("x")
            .and_then(Value::as_f64)
            .or(item.x)
            .ok_or_else(|| {
                anyhow::anyhow!("graphic has no anchor position; use rectangle geometry fields")
            })?;
        let y = args
            .get("y")
            .and_then(Value::as_f64)
            .or(item.y)
            .ok_or_else(|| {
                anyhow::anyhow!("graphic has no anchor position; use rectangle geometry fields")
            })?;
        let (ox, oy) = item
            .start_xy
            .unwrap_or((item.x.unwrap_or(0.0), item.y.unwrap_or(0.0)));
        edited = translate_block(
            &edited,
            &item.kind,
            valid_number(x, "x")? - ox,
            valid_number(y, "y")? - oy,
        )?;
    }
    if let Some(rotation) = args.get("rotation").and_then(Value::as_f64) {
        let rotation = valid_number(rotation, "rotation")?;
        let (s, e) = coord_span(&edited, "at")
            .ok_or_else(|| anyhow::anyhow!("graphic has no rotation-bearing anchor"))?;
        let (x, y, _) = coord_from_block(&edited, "at").unwrap();
        edited = apply_edits(
            edited,
            vec![SexpEdit::replace(
                s,
                e,
                coord_text("at", x, y, Some(rotation)),
            )],
        );
    }
    Ok(apply_edits(
        content.to_owned(),
        vec![SexpEdit::replace(item.start, item.end, edited)],
    ))
}

fn commit(path: &Path, old: &str, new: String) -> anyhow::Result<()> {
    write_atomic_if_unchanged(path, old, &new)?;
    Ok(())
}

async fn handle_list(args: &Value, _ctx: &ToolContext) -> anyhow::Result<CallToolResult> {
    let path = get_path(args, "schematic")?;
    let content = read_consistent(&path)?;
    let items: Vec<Value> = records(&content)
        .iter()
        .map(|r| output_item(r, &content))
        .collect();
    Ok(CallToolResult::json(
        &json!({"count":items.len(),"graphics":items}),
    ))
}

async fn handle_move(args: &Value, _ctx: &ToolContext) -> anyhow::Result<CallToolResult> {
    let path = get_path(args, "schematic")?;
    let uuid = match require_str(args, "uuid") {
        Ok(value) => value.to_owned(),
        Err(error) => return Ok(error),
    };
    let dx = match require_f64(args, "dx") {
        Ok(value) => valid_number(value, "dx")?,
        Err(error) => return Ok(error),
    };
    let dy = match require_f64(args, "dy") {
        Ok(value) => valid_number(value, "dy")?,
        Err(error) => return Ok(error),
    };
    let old = read_consistent(&path)?;
    let item = record(&old, &uuid)?;
    let replacement = translate_block(&old[item.start..item.end], &item.kind, dx, dy)?;
    commit(
        &path,
        &old,
        apply_edits(
            old.clone(),
            vec![SexpEdit::replace(item.start, item.end, replacement)],
        ),
    )?;
    Ok(CallToolResult::json(
        &json!({"moved":uuid,"dx":dx,"dy":dy,"uuid_preserved":true}),
    ))
}

async fn handle_edit(args: &Value, _ctx: &ToolContext) -> anyhow::Result<CallToolResult> {
    let path = get_path(args, "schematic")?;
    let uuid = match require_str(args, "uuid") {
        Ok(value) => value.to_owned(),
        Err(error) => return Ok(error),
    };
    let old = read_consistent(&path)?;
    let new = edit_content(&old, &uuid, args)?;
    commit(&path, &old, new)?;
    Ok(CallToolResult::json(
        &json!({"edited":uuid,"uuid_preserved":true}),
    ))
}

async fn handle_delete(args: &Value, _ctx: &ToolContext) -> anyhow::Result<CallToolResult> {
    let path = get_path(args, "schematic")?;
    let uuid = match require_str(args, "uuid") {
        Ok(value) => value.to_owned(),
        Err(error) => return Ok(error),
    };
    let old = read_consistent(&path)?;
    let item = record(&old, &uuid)?;
    commit(
        &path,
        &old,
        apply_edits(old.clone(), vec![SexpEdit::delete(item.start, item.end)]),
    )?;
    Ok(CallToolResult::json(&json!({"deleted":uuid})))
}

#[cfg(test)]
mod tests {
    use super::*;

    const FIXTURE: &str = include_str!("../../tests/fixtures/schematic_graphics.kicad_sch");

    #[test]
    fn lists_native_graphics_with_geometry_and_style() {
        let items = records(FIXTURE);
        assert_eq!(items.len(), 3);
        assert_eq!(items[0].kind, "rectangle");
        assert_eq!(items[0].start_xy, Some((10.0, 20.0)));
        assert!(items[0].style_tags.iter().any(|t| t == "stroke"));
        assert_eq!(items[1].text.as_deref(), Some("POWER"));
        assert_eq!(items[2].kind, "text_box");
    }

    #[test]
    fn moves_and_edits_by_uuid_without_changing_uuid() {
        let moved = translate_block(
            &FIXTURE[FIXTURE.find("(rectangle").unwrap()
                ..FIXTURE.find("(rectangle").unwrap()
                    + find_balanced_block(&FIXTURE[FIXTURE.find("(rectangle").unwrap()..], 0)
                        .unwrap()
                        .1],
            "rectangle",
            5.0,
            6.0,
        )
        .unwrap();
        assert!(moved.contains("(start 15 26)"));
        let edited = edit_content(
            FIXTURE,
            "text-1",
            &json!({"text":"VCC RAIL","x":55.0,"y":66.0,"rotation":90.0}),
        )
        .unwrap();
        assert!(edited.contains("VCC RAIL"));
        assert!(edited.contains("(uuid \"text-1\")"));
        assert!(edited.contains("(at 55 66 90)"));
    }

    #[test]
    fn delete_targets_only_the_requested_uuid() {
        let item = record(FIXTURE, "box-1").unwrap();
        let out = apply_edits(
            FIXTURE.to_owned(),
            vec![SexpEdit::delete(item.start, item.end)],
        );
        assert!(!out.contains("box-1"));
        assert!(out.contains("rect-1"));
        assert!(out.contains("text-1"));
    }
}
