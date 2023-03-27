import os, sys

def generate_html(resource_dir, nightly_dir):
    # resources
    index_html = os.path.join(resource_dir, "index_base.html")
    base_html =  os.path.join(resource_dir, "base.html")

    # nightly output
    output_json = os.path.join(nightly_dir, "json/output.json")
    derive_dir =  os.path.join(nightly_dir, "json/derivable_rules/")
    output_dir =  os.path.join(nightly_dir, "output/")

    # Create the index page
    json = open(output_json, "r").read()
    html = open(index_html, "r").read()
    html = html.replace("\"REPLACE_WITH_JSON\"", json)
    open(output_dir + "index.html", "w").write(html)

    # loop through derivable_rules
    # generate HTML files for each one
    for filename in os.listdir(derive_dir):
        if filename.endswith(".json"):
            path = os.path.join(derive_dir, filename)
            content = open(path, "r").read()
            base = open(base_html, "r").read()
            base = base.replace("NAME", filename)
            base = base.replace("\"REPLACE_WITH_JSON\"", content)

            (root, _) = os.path.splitext(filename)
            open(output_dir + "/" + root + ".html", "w").write(base)

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Expected: `python generatehtml.py [RESOURCE_DIR] [NIGHTLY_DIR]`")
        print("Received:", sys.argv[1:])
        print("Explanation:")
        print(" RESOURCE_DIR - directory containing template files for nightly report generation")
        print(" NIGHTLY_DIR  - directory containing a partially generated nightly report")
        sys.exit(1)
    generate_html(sys.argv[1], sys.argv[2])
