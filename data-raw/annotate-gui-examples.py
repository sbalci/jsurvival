#!/usr/bin/env python3
"""Maintain native jamovi teaching notes without changing data or analysis results.

Run from any directory: python3 data-raw/annotate-gui-examples.py [--check]
Only the first jmv::empty analysis is updated, using jamovi's native heading and
Quill topText options. Open and save in jamovi afterwards to refresh index.html,
the cached HTML export. This script deliberately preserves that export and all
data, fitted analyses, results, figures, and other archive members byte for byte.

The small protobuf wire reader avoids a Python protobuf dependency. It retains
unknown AnalysisResponse fields and changes only the named native note options.
"""

from __future__ import annotations

import argparse
import copy
import json
import os
from pathlib import Path
import re
import tempfile
import zipfile


def varint(value: int) -> bytes:
    result = bytearray()
    while value > 127:
        result.append((value & 127) | 128)
        value >>= 7
    result.append(value)
    return bytes(result)


def read_varint(data: bytes, offset: int) -> tuple[int, int]:
    value = 0
    for shift in range(0, 70, 7):
        if offset >= len(data):
            raise ValueError("Truncated protobuf varint")
        part = data[offset]
        offset += 1
        value |= (part & 127) << shift
        if part < 128:
            return value, offset
    raise ValueError("Invalid protobuf varint")


def fields(data: bytes) -> list[tuple[int, int, bytes, bytes]]:
    """Return (field number, wire type, payload, complete original bytes)."""
    result = []
    offset = 0
    while offset < len(data):
        start = offset
        key, offset = read_varint(data, offset)
        number, wire = key >> 3, key & 7
        if number == 0:
            raise ValueError("Invalid protobuf field number")
        if wire == 0:
            payload_start = offset
            _, offset = read_varint(data, offset)
            payload = data[payload_start:offset]
        elif wire in (1, 5):
            end = offset + (8 if wire == 1 else 4)
            payload, offset = data[offset:end], end
        elif wire == 2:
            size, offset = read_varint(data, offset)
            end = offset + size
            payload, offset = data[offset:end], end
        else:
            raise ValueError(f"Unsupported protobuf wire type {wire}")
        if offset > len(data):
            raise ValueError("Truncated protobuf payload")
        result.append((number, wire, payload, data[start:offset]))
    return result


def message_field(number: int, value: bytes) -> bytes:
    return varint((number << 3) | 2) + varint(len(value)) + value


def string_field(number: int, value: str) -> bytes:
    return message_field(number, value.encode("utf-8"))


def option(value: str | list | dict) -> bytes:
    """Encode the AnalysisOption types used by native Quill annotations."""
    if isinstance(value, str):
        return string_field(3, value)
    if isinstance(value, dict):
        entries = list(value.items())
        contents = b"".join(message_field(1, option(v)) for _, v in entries)
        contents += b"\x10\x01"  # AnalysisOptions.hasNames = true
        contents += b"".join(string_field(3, k) for k, _ in entries)
        return message_field(5, contents)
    if isinstance(value, list):
        contents = b"".join(message_field(1, option(v)) for v in value)
        return message_field(5, contents)
    raise TypeError(f"Unsupported annotation option: {type(value).__name__}")


def scalar_string(data: bytes, number: int) -> str:
    values = [p.decode("utf-8") for n, w, p, _ in fields(data) if n == number and w == 2]
    if len(values) > 1:
        raise ValueError(f"Duplicate protobuf string field {number}")
    return values[0] if values else ""


def update_native_options(original: bytes, title: str, paragraphs: list[str]) -> bytes:
    parsed = fields(original)
    values = [p for n, w, p, _ in parsed if n == 1 and w == 2]
    names = [p.decode("utf-8") for n, w, p, _ in parsed if n == 3 and w == 2]
    if len(values) != len(names) or len(set(names)) != len(names):
        raise ValueError("Native note options must have unique matching names and values")
    replacements = {
        "results//heading": option(title),
        "results//topText": option({"ops": [{"insert": "\n\n".join(paragraphs) + "\n"}]}),
    }
    for name, value in replacements.items():
        if name in names:
            values[names.index(name)] = value
        else:
            names.append(name)
            values.append(value)
    result = b"".join(message_field(1, value) for value in values)
    result += b"\x10\x01"
    result += b"".join(string_field(3, name) for name in names)
    result += b"".join(raw for n, _, _, raw in parsed if n not in (1, 2, 3))
    return result


def annotate_response(original: bytes, title: str, paragraphs: list[str]) -> bytes:
    if scalar_string(original, 3) != "empty" or scalar_string(original, 4) != "jmv":
        raise ValueError("The selected introduction must be a native jmv::empty analysis")
    parsed = fields(original)
    options = [p for n, w, p, _ in parsed if n == 6 and w == 2]
    if len(options) > 1:
        raise ValueError("Duplicate AnalysisResponse.options")
    replacement = message_field(6, update_native_options(options[0] if options else b"",
                                                       title, paragraphs))
    if options:
        return b"".join(replacement if n == 6 else raw for n, _, _, raw in parsed)
    return original + replacement


def update_workbook(path: Path, tutorial: dict, check: bool) -> bool:
    with zipfile.ZipFile(path) as source:
        if source.testzip() is not None:
            raise ValueError(f"Invalid ZIP member in {path}")
        members = source.infolist()
        introductions = [m for m in members if re.fullmatch(r"\d+ empty/analysis", m.filename)]
        if not introductions:
            raise ValueError(f"No native introduction analysis in {path}")
        first = introductions[0]
        analysis_members = [m for m in members if m.filename.endswith("/analysis")]
        if not any(scalar_string(source.read(m), 3) == tutorial["function"] and
                   scalar_string(source.read(m), 4) == "jsurvival" for m in analysis_members):
            raise ValueError(f"Missing saved jsurvival::{tutorial['function']} analysis in {path}")
        original = source.read(first)
        updated = annotate_response(original, tutorial["heading"], tutorial["paragraphs"])
        if original == updated:
            print(f"CURRENT {path.name}")
            return False
        if check:
            print(f"OUTDATED {path.name}")
            return True
        descriptor, temporary_name = tempfile.mkstemp(prefix=f".{path.name}.", dir=path.parent)
        os.close(descriptor)
        temporary = Path(temporary_name)
        try:
            with zipfile.ZipFile(temporary, "w") as destination:
                destination.comment = source.comment
                for member in members:
                    payload = updated if member.filename == first.filename else source.read(member)
                    destination.writestr(copy.copy(member), payload)
            with zipfile.ZipFile(temporary) as destination:
                if destination.testzip() is not None:
                    raise ValueError(f"Invalid output archive: {temporary}")
                if destination.namelist() != source.namelist():
                    raise ValueError("Archive membership changed")
                for member in members:
                    if (member.filename != first.filename and
                            destination.read(member.filename) != source.read(member.filename)):
                        raise ValueError(f"Unexpected changed member: {member.filename}")
            os.chmod(temporary, path.stat().st_mode)
        except BaseException:
            temporary.unlink(missing_ok=True)
            raise
    os.replace(temporary, path)
    print(f"ANNOTATED {path.name}")
    return True


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true", help="Read only; exit 1 if notes need updating")
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[1],
                        help="jsurvival repository root (defaults to this script's parent)")
    args = parser.parse_args()
    tutorials = json.loads((args.root / "data-raw/gui-example-tutorials.json").read_text())
    required = {"singlearm", "survival", "survivalcont", "multisurvival", "lassocox",
                "oddsratio", "datetimeconverter", "outcomeorganizer", "timeinterval"}
    if ({x["function"] for x in tutorials} != required or
            len({x["file"] for x in tutorials}) != len(tutorials)):
        raise ValueError("Expected unique workbook tutorials covering all nine shipped analyses")
    changed = sum(update_workbook(args.root / "data" / x["file"], x, args.check) for x in tutorials)
    print(f"{'Outdated' if args.check else 'Updated'} {changed} of {len(tutorials)} teaching introductions.")
    if args.check and changed:
        raise SystemExit(1)


if __name__ == "__main__":
    main()
