from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()


class Elf:
    def __init__(self, n):
        self.n = n
        self.next_elf = None

    def __repr__(self):
        return str(self.n)

start_elf = Elf(1)
current_elf = start_elf
for i in range(2, int(puzzle_input_raw) + 1):
    current_elf.next_elf = Elf(i)
    current_elf = current_elf.next_elf
current_elf.next_elf = start_elf

elf = start_elf
while elf.next_elf != elf:
    elf.next_elf = elf.next_elf.next_elf
    elf = elf.next_elf

print(elf)