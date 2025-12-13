#!/usr/bin/python3

import csv, os, re, sys
from pathlib import Path

out_dir = Path("out")
testing_re = re.compile(r"@@>[ ]+Testing ([\-0-9a-zA-Z]+) over ([\-_0-9a-zA-Z]+):.*")
verify_re = re.compile(r"@@[ ]+([\-0-9a-zA-Z]+) verify: ([0-9a-zA-Z]+)")
output_qc_split_re = re.compile(r"([\-0-9a-zA-Z]+)_([\-_0-9a-zA-Z]+).qc")
time_txt_split_re = re.compile(
    r"([\-0-9a-zA-Z]+)_((_(?!time.txt$)|[\-_0-9a-zA-Z])+)_time.txt"
)
parse_times_re = re.compile(
    r"([.:0-9]+)user ([.:0-9]+)system [.:0-9]+elapsed [.0-9]+%CPU \([0-9]+avgtext\+[0-9]+avgdata ([0-9]+)maxresident\)k"
)
for run_dir in filter(lambda x: x.is_dir(), out_dir.iterdir()):
    methods = set()
    inputs = set()

    all_attrs = dict()

    def set_attr_val(
        method, input, attr, value, methods=methods, inputs=inputs, all_attrs=all_attrs
    ):
        methods.add(method)
        inputs.add(input)
        method_attrs = all_attrs.setdefault(method, dict())
        method_input_attrs = method_attrs.setdefault(input, dict())
        method_input_attrs[attr] = value

    def get_attr_val(method, input, attr, defval, all_attrs=all_attrs):
        method_attrs = all_attrs.get(method, dict())
        method_input_attrs = method_attrs.get(input, dict())
        return method_input_attrs.get(attr, defval)

    with open(run_dir / "benchmark.log", "rt") as f:
        method = None
        input = None
        for l in f.readlines():
            m = testing_re.match(l)
            if m:
                method = m.group(1)
                input = m.group(2)
                continue
            m = verify_re.match(l)
            if m:
                v_method = m.group(1)
                if method is None or v_method != method:
                    print("Bad benchmark.log in {run_dir.name}")
                set_attr_val(method, input, "valid", m.group(2))
                continue

    for output_qc in run_dir.glob("*.qc"):
        m = output_qc_split_re.match(output_qc.name)
        if not m:
            print(
                f"Bad output filename format for '{output_qc.name}': "
                f"expected '<method>_<input>.qc"
            )
            sys.exit(1)
        method = m.group(1)
        input = m.group(2)
        cnot_count = len(
            [
                l
                for l in open(output_qc, "rt").readlines()
                if l.lstrip().startswith("cnot ")
            ]
        )
        set_attr_val(method, input, "cnots", cnot_count)

    for time_txt in run_dir.glob("*_time.txt"):
        m = time_txt_split_re.match(time_txt.name)
        if not m:
            print(
                f"Bad output filename format for '{time_txt.name}': "
                f"expected '<method>_<input>_time.txt"
            )
            sys.exit(1)
        method = m.group(1)
        input = m.group(2)
        for l in open(time_txt, "rt").readlines():
            m = parse_times_re.match(l.lstrip())
            if m:
                user_time = float(m.group(1))
                sys_time = float(m.group(2))
                max_resident_k = float(m.group(3))
                set_attr_val(method, input, "user", user_time)
                set_attr_val(method, input, "sys", sys_time)
                set_attr_val(method, input, "maxresk", max_resident_k)

    def write_attrs(attr, defval, w):
        w.writerow([""] + sorted(methods))
        for input in sorted(inputs):
            w.writerow(
                [input]
                + [
                    get_attr_val(method, input, attr, defval)
                    for method in sorted(methods)
                ]
            )

    write_attrs("user", "", csv.writer(open(run_dir / "!user.csv", "wt")))
    write_attrs("sys", "", csv.writer(open(run_dir / "!sys.csv", "wt")))
    write_attrs("maxresk", "", csv.writer(open(run_dir / "!maxresk.csv", "wt")))
    write_attrs("cnots", "", csv.writer(open(run_dir / "!cnots.csv", "wt")))
