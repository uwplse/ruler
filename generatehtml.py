import sys, os

baseline_json = "rep/json/baseline.json"
herbie_json = "rep/json/herbie.json"
halide_json = "rep/json/halide.json"

index_html = "rep/index_base.html"
derive_dir = "rep/json/derivable_rules"
output_dir = "rep/output"

def generate_html():
    baseline = open(baseline_json, "r").read()
    herbie = open(herbie_json, "r").read()
    halide = open(halide_json, "r").read()

    html = open(index_html, "r").read()
    html = html.replace("\"REPLACE_WITH_BASELINE\"", baseline)
    html = html.replace("\"REPLACE_WITH_HERBIE\"", herbie)
    html = html.replace("\"REPLACE_WITH_HALIDE\"", halide)

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
          without_extension = filename[:-5]
          base = base.replace("\"REPLACE_WITH_SPECNAME\"", without_extension)
          base = base.replace("\REPLACE_WITH_FILENAME\"", filename)
          
          open(output_dir + "/" + without_extension + ".html", "w").write(base)
          open(output_dir + "/" + filename, "w").write(content)

generate_html()