#!/usr/bin/env python


import codecs
import os

import lxml.etree as ET


## CHANGE THIS:
INPUT_DIR = 'gk/Classics/Plato'
OUTPUT_DIR = 'plato-text'
## Then, input the directory into mallet:
## > bin/mallet --input-dir dialogs
## See http://programminghistorian.org/lessons/topic-modeling-and-mallet


def get_text(el, buf):
    """Return all the text content for this element."""
    for child in el:
        if child.text:
            buf.append(child.text)
        get_text(child, buf)
        if child.tail:
            buf.append(child.tail)


def iter_file_text(filename):
    """Return the text for a file."""
    print('Parsing {}...'.format(filename))
    parser = ET.XMLParser(dtd_validation=True, no_network=False)
    xml    = ET.parse(filename, parser)

    n = 0
    for text in xml.findall('.//group/text'):
        n += 1

        # walk through `text`, getting all the text content
        buffer = []
        get_text(text, buffer)
        text_content = ' '.join(buffer)

        yield text_content


def save_file_texts(filename, output_prefix):
    """Iterate over the file's texts and save to files with a given prefix."""
    output_template = output_prefix + '-{:04d}.txt'
    for (i, text) in enumerate(iter_file_text(filename)):
        filename = output_template.format(i)

        # put the text content into the file
        with codecs.open(filename, 'w', encoding='utf8') as f:
            f.write(text)

        # print filename
        print(filename)


def main():
    if not os.path.isdir(OUTPUT_DIR):
        os.makedirs(OUTPUT_DIR)

    output_base = os.path.join(OUTPUT_DIR, 'text')
    for (root, dirs, files) in os.walk(INPUT_DIR):
        for fn in files:
            full_fn = os.path.join(root, fn)

            save_file_texts(full_fn, output_base)

    print('done.')


if __name__ == '__main__':
    main()
