import os
import re
import sys
import tarfile


files = []
summary_files = []
punctuation_regex = r"\(|\)|,|(\->)|\.|{|}|\[|\]|\*|:|\'|&|!|=|(\-\-)|(\+\+)"
identifier_regex = r"([A-Za-z_]+[A-Za-z0-9_\-\?]*)"
string_regex = r"\"([^\"\\]|\\.)*\""
c_comment = r"(\/\/[^\n]*\n)"
clj_comment = r"(;[^\n]*\n)"
scala_comment = r"(\/\/[^\n]*\n)"
pro_comment = r"(%[^\n]*\n)"
py_comment = r"(#[^\n]*\n)"


class File:
    def __init__(self, name, path, num_lines, identifiers):
        self.name = name
        self.path = path
        self.num_lines = str(num_lines[0])
        self.identifiers = identifiers

    def to_string(self):
        ret = "\t<div>\n"
        ret += "\t\t<h2>" + self.name + "</h2>\n"
        link = str(self.name).replace(" ", "%20")
        ret += "\t\t<p>File: <a href=" + link + ">" + self.name + "</a></p>\n"
        ret += "\t\t<p>Number of Lines: " + self.num_lines + "</p>\n"
        ret += "\t\t<p>Identifiers:</p>\n"
        ret += "\t\t<ul>\n"
        for identifier in self.identifiers:
            ret += "\t\t\t<li>" + identifier + "</li>\n"
        ret += "\t\t</ul>\n"
        ret += "\t</div>\n"
        return ret


def create_html(title, content):
    html = "<!DOCTYPE html>\n\n"
    html += "<html>\n"
    html += "<head>\n"
    html += "\t<title>" + title + "</title>\n"
    html += "</head>\n\n"
    html += "<body>\n"
    html += content
    html += "</body>"
    return html


def create_summary(file):
    content = create_html("File Information", file.to_string())
    file_name = file.path + "/summary_" + os.path.basename(file.path) + ".html"
    if not summary_files.__contains__(file_name):
        summary_files.append(file_name)
    f = open(file_name, 'w')
    f.write(content)
    f.close()


def get_identifiers(file_content, file_type):
    # Certain characters have to be filtered out later due to comments in certain languages
    file_content = re.sub(punctuation_regex, " ", file_content)
    file_content = file_content.replace("r\"", "\"")
    file_content = re.sub(string_regex, "", file_content)
    if file_type == "c":
        file_content = file_content.replace(";", " ")
        file_content = re.sub(c_comment, "", file_content)
    if file_type == "clj":
        file_content = file_content.replace("/", " ")
        file_content = re.sub(clj_comment, "", file_content)
    if file_type == "scala":
        file_content = re.sub(scala_comment, "", file_content)
    if file_type == "pro":
        file_content = re.sub(pro_comment, "", file_content)
    if file_type == "py":
        file_content = re.sub(py_comment, "", file_content)
    identifiers = []
    words = file_content.split()
    for i in range(0, len(words)):
        if re.fullmatch(identifier_regex, words[i]):
            if not words[i] in identifiers:
                identifiers.append(words[i])
    identifiers.sort()
    return identifiers


def get_files(path):
    sorted_files = os.listdir(path)
    sorted_files.sort()
    for file_name in sorted_files:
        if not file_name[0] == '.':  # No hidden files
            if file_name.startswith("summary"):  # This messes it up if its still there from last time
                os.remove(path + "/" + file_name)
            elif file_name.startswith("index"):  # This messes it up if its still there from last time
                os.remove(path + "/" + file_name)
            elif file_name.__contains__(".tar.gz"):  # This messes it up if its still there from last time
                os.remove(path + "/" + file_name)
            elif not os.path.isdir(path + "/" + file_name):
                metadata = os.popen("wc -l \"" + path + "/" + file_name + "\"").read()
                num_lines = [int(i) for i in metadata.split() if i.isdigit()]
                file_type = file_name.split(".")[1]
                identifiers = get_identifiers(open((path + "/" + file_name), 'r').read(), file_type)
                if num_lines:
                    files.append(File(file_name, path, num_lines, identifiers))
            else:  # Recursively get nested directories
                get_files(path + "/" + file_name)


def create_index(dir):
    html = "\t<div>\n"
    html += "\t\t<h2>Summaries</h2>\n"
    html += "\t\t<ul>\n"
    for path in summary_files:
        link = os.path.basename(os.path.dirname(path)).replace(" ", "%20")  # Need to get parent folder
        link += "/" + os.path.basename(path).replace(" ", "%20")
        html += "\t\t\t<li><a href=" + link + ">" + os.path.basename(path) + "</a></li>\n"
    html += "\t\t</ul>\n"
    html += "\t</div>\n"
    content = create_html("Index", html)
    f = open((dir + "/index.html"), 'w')
    f.write(content)
    f.close()


if len(sys.argv) > 1:
    os.chdir(os.path.expanduser(sys.argv[1]))
    base_dir = os.getcwd()

    get_files(base_dir)

    for file in files:
        create_summary(file)

    create_index(base_dir)

    tar_name = os.path.basename(base_dir) + ".tar.gz"
    tar = tarfile.open(tar_name, "w:gz")
    for name in os.listdir(base_dir):
        tar.add(name)
    tar.close()

    email_address = input("Please enter an email address: ")
    if not email_address == "none":
        email_subject = "Project 5 Email"
        email_content = input("Please enter the email body: ")
        os.system("echo \"" + email_content + "\" | mutt -s \"" + email_subject + "\" " + email_address + " -a " + tar_name)
else:
    print("You need to provide a directory")
