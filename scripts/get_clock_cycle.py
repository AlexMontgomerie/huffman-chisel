import re
import sys
import os
from tabulate import tabulate

header = ["module", "test", "cycles"]
table = []

# get clock cycles of a vcd file
def get_clock_cycles(filename):
    # open file and find the number of clock cycles
    for line in reversed(list(open(filename))):
        clk_cycle = re.compile("#([0-9]+)")
        if clk_cycle.match(line):
            total_cycles = clk_cycle.match(line).group(1)
            return int(float(total_cycles)/2.0)

# iterate over outputs
for filepath in os.listdir("test_run_dir"):
    for file in os.listdir(f"test_run_dir/{filepath}"):
        if file.endswith(".vcd"):
            module = file.rsplit('.', 1)[0]
            test = filepath
            cycles = get_clock_cycles(f"test_run_dir/{filepath}/{file}")
            table.append((module, test, cycles))

# print table
print(tabulate(
    table,
    headers=header
))
