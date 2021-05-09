import argparse
import fileinput
from subprocess import call

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("file_name", help="name of ipynb file")
    args = parser.parse_args()

    markdown = args.file_name[:-6] + ".md"
    org = args.file_name[:-6] + "_old.org"

    # remove outputs and convert to markdown
    call(["jupyter", "nbconvert", args.file_name, "--clear-output"])
    call(["jupyter", "nbconvert", args.file_name, "--to", "markdown"])

    # convert to org
    call(["pandoc", markdown, "-o", org])

    # remove markdown file
    call(["rm", markdown])

    # remove unnecessary lines
    bad_words = [":PROPERTIES:", ":CUSTOM_ID:", ":END:"]
    org_new = args.file_name[:-6] + ".org"
    with open(org) as oldfile, open(org_new, 'w') as newfile:
        for line in oldfile:
            if not any(bad_word in line for bad_word in bad_words):
                newfile.write(line)

    # replace org headers with correct headers
    with fileinput.FileInput(org_new, inplace=True) as file:
        for line in file:
            print(line.replace("#+BEGIN_SRC python", "#+begin_src jupyter-python"), end='')
    with fileinput.FileInput(org_new, inplace=True) as file:
        for line in file:
            print(line.replace("#+END_SRC", "#+end_src"), end='')
    with fileinput.FileInput(org_new, inplace=True) as file:
        for line in file:
            print(line.replace("#+begin_example", "#+begin_src jupyter-python"), end='')
    with fileinput.FileInput(org_new, inplace=True) as file:
        for line in file:
            print(line.replace("#+end_example", "#+end_src"), end='')

    # remove old org file
    call(["rm", org])
