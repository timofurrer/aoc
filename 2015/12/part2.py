from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()

import json

document = json.loads(puzzle_input_raw)

def count(doc):
    if isinstance(doc, int):
        return doc
    
    if isinstance(doc, str):
        return 0
    
    if isinstance(doc, list):
        return sum(count(d) for d in doc)

    if isinstance(doc, dict):
        if "red" in doc.values():
            return 0

        return sum(count(d) for d in doc.values())

    assert "unknown element", doc

print(count(document))