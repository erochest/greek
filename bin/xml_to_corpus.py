#!/usr/bin/env python


import codecs
import os

import lxml.etree as ET


## CHANGE THIS:
INPUT_FILE = 'Classics/Plato/opensource/plat.tet3_eng.xml'
OUTPUT_DIR = 'dialogs'
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


def main():
    if not os.path.isdir(OUTPUT_DIR):
        os.makedirs(OUTPUT_DIR)

    parser = ET.XMLParser(dtd_validation=True, no_network=False)
    xml    = ET.parse(INPUT_FILE, parser)

    n = 0
    for text in xml.findall('.//group/text'):
        n += 1

        # walk through `text`, getting all the text content
        buffer = []
        get_text(text, buffer)
        text_content = ' '.join(buffer)

        # make up a filename under OUTPUT_DIR
        filename = os.path.join(OUTPUT_DIR, 'text-%04d.txt' % (n,))

        # put the text content into the file
        with codecs.open(filename, 'w', encoding='utf8') as f:
            f.write(text_content)

        # print filename
        print(filename)

    print('done.')


if __name__ == '__main__':
    main()
