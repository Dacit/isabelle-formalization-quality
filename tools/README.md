# Tools
Isabelle tools used.

## Installation
1. clone isabelle + afp + this repository
2. add afp and all tools as components
3. run with `isabelle <TOOL>`:

Analysis:
- get lints:
```
isabelle/bin/isabelle lint -v -r count -o export_theory -o lints_warning=apply_isar_switch,bad_style_command,counter_example_finder,global_attribute_changes,global_attribute_on_unnamed_lemma,low_level_apply_chain,smt_oracle,unfinished_proof,unrestricted_auto -X slow -d '$AFP' -a -j 8 -O lint-count.csv
```

- count lines:
```
isabelle/bin/isabelle lines -v -o export_theory -X slow -d '$AFP' -a -j 8 -O line-count
```

- get metrics:
```
isabelle/bin/isabelle graph_import -o export_theory -X slow -d '$AFP' -a localhost:7687
```
```
isabelle/bin/isabelle graph_analysis -o export_theory -X slow -d '$AFP' -a -O analysis/ -j 8 -A pos_metrics localhost:7687
```

- build subgraphs:
```
isabelle/bin/isabelle graph_analysis -o export_theory -X slow -d '$AFP' -a -O analysis/ -j 8 -A store localhost:7687
```