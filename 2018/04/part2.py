from pathlib import Path

puzzle_input_raw = (Path(__file__).parent / "input.txt").read_text()

import re
from datetime import datetime
from collections import defaultdict, Counter

schedule = []
for l in puzzle_input_raw.splitlines():
    timestamp = datetime.strptime(l[1:17], "%Y-%m-%d %H:%M")
    schedule.append((timestamp, l[18:].strip()))

guards_last_asleep = {}
guards_asleep_minutes = defaultdict(list)
guards_asleep_durations = defaultdict(int)
current_guard = None
for timestamp, guard_action in sorted(schedule, key=lambda x: x[0]):
    if guard_action.startswith("Guard #"):
        current_guard = re.findall(r"(\d+)", guard_action)[0]
    elif guard_action == "falls asleep":
        guards_last_asleep[current_guard] = timestamp
    elif guard_action == "wakes up":
        asleep_timespan = (guards_last_asleep[current_guard], timestamp)
        guards_asleep_minutes[current_guard].extend(range(asleep_timespan[0].minute, asleep_timespan[1].minute))
        guards_asleep_durations[current_guard] += int((asleep_timespan[1] - asleep_timespan[0]).total_seconds()) // 60


sleepiest_guard, (sleepiest_minute, _) = max({k: Counter(v).most_common(1)[0] for k, v in guards_asleep_minutes.items()}.items(), key=lambda x: x[1][1])
print(int(sleepiest_guard) * sleepiest_minute)