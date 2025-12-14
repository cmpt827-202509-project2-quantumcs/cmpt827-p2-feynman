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

def format_number(value, metric):
    """Format numbers appropriately based on metric type."""
    try:
        num = float(value)
        if metric in ["user", "sys"]:
            return f"{num:.2f}"  # Time in seconds with 2 decimal places
        elif metric == "maxresk":
            # Convert KB to MB for readability
            mb = num / 1024
            return f"{mb:.1f}"
        else:
            return str(int(num))  # CNOT count as integer
    except (ValueError, TypeError):
        return "---"

def generate_latex_table(methods, inputs, all_attrs):
    """Generate a combined LaTeX table from the collected data."""
    
    # Build LaTeX table
    latex = []
    latex.append("% LaTeX table generated from benchmark results")
    latex.append("% Uses table* for two-column documents\n")
    
    latex.append("\\begin{table*}[htbp]")
    latex.append("\\centering")
    latex.append("\\caption{Benchmark Results: User Time (s), System Time (s), Max Memory (MB), and CNOT Count}")
    latex.append("\\label{tab:benchmark_results}")
    latex.append("\\resizebox{\\textwidth}{!}{%")
    
    # Column specification with vertical borders
    sorted_methods = sorted(methods)
    col_spec = "|l|" + "|".join(["rrrr"] * len(sorted_methods)) + "|"
    latex.append(f"\\begin{{tabular}}{{{col_spec}}}")
    latex.append("\\hline")
    
    # Multi-row header
    # First header row: method names spanning 4 columns each
    header_row1 = "\\textbf{Benchmark}"
    for method in sorted_methods:
        header_row1 += f" & \\multicolumn{{4}}{{c|}}{{\\textbf{{{method}}}}}"
    latex.append(header_row1 + " \\\\")
    latex.append("\\hline")
    
    # Second header row: metric labels for each method
    header_row2 = ""
    for _ in sorted_methods:
        header_row2 += " & \\textbf{User} & \\textbf{Sys} & \\textbf{Mem} & \\textbf{CNOTs}"
    latex.append(header_row2 + " \\\\")
    latex.append("\\hline")
    
    # Data rows
    for input_name in sorted(inputs):
        row = [input_name.replace("_", "\\_")]
        
        for method in sorted_methods:
            method_attrs = all_attrs.get(method, {})
            input_attrs = method_attrs.get(input_name, {})
            
            user_val = input_attrs.get("user", "")
            sys_val = input_attrs.get("sys", "")
            mem_val = input_attrs.get("maxresk", "")
            cnot_val = input_attrs.get("cnots", "")
            
            row.append(format_number(user_val, "user"))
            row.append(format_number(sys_val, "sys"))
            row.append(format_number(mem_val, "maxresk"))
            row.append(format_number(cnot_val, "cnots"))
        
        latex.append(" & ".join(row) + " \\\\")
        latex.append("\\hline")
    
    latex.append("\\end{tabular}%")
    latex.append("}")
    latex.append("\\end{table*}")
    
    return "\n".join(latex)

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
    
    # Generate LaTeX table
    latex_content = generate_latex_table(methods, inputs, all_attrs)
    latex_file = run_dir / "benchmark_table.tex"
    with open(latex_file, "wt") as f:
        f.write(latex_content)