from pathlib import Path

with (Path(__file__).parent / "input.txt").open() as puzzle_input_file:
    puzzle_input_raw = puzzle_input_file.read()


from typing import Self
from dataclasses import dataclass


@dataclass
class File:
    size: int


@dataclass
class Dir:
    name: str
    parent: Self
    entries: list[Self, File]

    @property
    def size(self):
        return sum(x.size for x in self.entries)


dirs = [] 
cwd = None
for parts in (l.split() for l in puzzle_input_raw.splitlines()):
    if parts[0] == "$":
        if parts[1] == "cd":
            if parts[2] == "..":
                cwd = cwd.parent
            else:
                d = Dir(parts[2], cwd, entries=[])
                dirs.append(d)
                if cwd is not None:
                    cwd.entries.append(d)
                cwd = d
    elif parts[0] != "dir":
        cwd.entries.append(File(int(parts[0])))


print(sum(s for x in dirs if (s := x.size) <= 100_000))