bash scripts.sh -d bool -o ablation/data
bash scripts.sh -d bv4ns -o ablation/data
node parse.json
python visualize.py