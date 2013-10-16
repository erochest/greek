#!/usr/bin/env python


"""\
This breaks the files in INPUT_DIR into chunks of CHUNK_SIZE, windowed by
OFFSET.

NB: OUTPUT_DIR will be destroyed and recreated."""


import argparse
import os
import shutil
import sys


def tokenize(text):
    return text.split()


def explode_chunks(chunk_size, offset, input_file):
    with open(input_file) as f:
        tokens = tokenize(f.read())
    indexes = range(len(tokens))
    for start in indexes[::offset]:
        end = start + chunk_size
        yield ' '.join(tokens[start:end])


def parse_args(argv):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
            'input_dir', type=str, default='dialogs', metavar='INPUT_DIR',
            help='The directory to read for input files.',
            )
    parser.add_argument(
            'output_dir', type=str, default='chunks', metavar='OUTPUT_DIR',
            help='The directory to put the output chunks into.',
            )
    parser.add_argument(
            'chunk_size', type=int, default=500, metavar='CHUNK_SIZE',
            help='The size of the chunks to create, in rough tokens '
                 '(default=500).',
            )
    parser.add_argument(
            'offset', type=int, default=250, metavar='OFFSET',
            help='The offset for each windowed chunk (default=250).',
            )
    args = parser.parse_args()
    return args


def main(argv=None):
    opts       = parse_args(argv or sys.argv[1:])
    input_dir  = os.path.abspath(opts.input_dir)
    output_dir = os.path.abspath(opts.output_dir)

    if os.path.exists(output_dir):
        print('deleting {0}...'.format(output_dir))
        shutil.rmtree(opts.output_dir)

    for (root, _, files) in os.walk(input_dir):
        for fn in files:
            fullfn    = os.path.join(root, fn)
            shared    = os.path.commonprefix([input_dir, fullfn])
            rest      = fullfn[len(shared)+1:]
            (base, _) = os.path.splitext(rest)
            base      = os.path.join(output_dir, base)
            chunks    = explode_chunks(
                    opts.chunk_size,
                    opts.offset,
                    fullfn,
                    )

            print(fullfn)
            os.makedirs(base)
            for (i, chunk) in enumerate(chunks):
                outfile = os.path.join(base, 'chunk-%04d.txt' % (i,))
                print("\t=> {0}".format(outfile))
                with open(outfile, 'w') as f:
                    f.write(chunk)

    print('done.')


if __name__ == '__main__':
    main()
