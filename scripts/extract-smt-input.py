#!/usr/bin/env python3

import argparse
import sys

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('file', type=argparse.FileType('r'))
    parser.add_argument('context', type=str)
    parser.add_argument('-r', '--recv', action='store_true')
    args = parser.parse_args()

    file = args.file
    context = args.context
    recv = args.recv

    dir_sep = '[{}] '.format('recv' if recv else 'send')

    for line in file:
        chunk = drop_through(line, [context, dir_sep])
        if chunk is not None:
            sys.stdout.write(chunk)

def drop_through(s, seps):
    for sep in seps:
        parts = s.split(sep, maxsplit=1)
        if len(parts) < 2:
            return None
        s = parts[1]
    return s

if __name__ == '__main__':
    main()
