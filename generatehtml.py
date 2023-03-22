import sys, os
from pathlib import Path

output_json = "rep/json/output.json"
index_html = "rep/index_base.html"
derive_dir = "rep/json/derivable_rules"
output_dir = "rep/output"

def generate_html():
    json = open(output_json, "r").read()
    html = open(index_html, "r").read()
    html = html.replace("\"REPLACE_WITH_JSON\"", json)
    open(output_dir + "/index.html", "w").write(html)

    # loop through derivable_rules
    # generate HTML files for each one
    for filename in os.listdir(derive_dir):
        if filename.endswith(".json"):
          path = os.path.join(derive_dir, 
          filename)
          content = open(path, "r").read()
          base = open("rep/base.html", "r").read()
          base = base.replace("NAME", filename)
          base = base.replace("\"REPLACE_WITH_JSON\"", content)

          specname = Path(filename)
          base = base.replace("\"REPLACE_WITH_SPECNAME\"", specname.stem)
          without_extension = filename[:-5]
          
          open(output_dir + "/" + without_extension + ".html", "w").write(base)


generate_html()