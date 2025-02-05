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
    sep = context + ' [{}] '.format('recv' if recv else 'send')

    for line in file:
        parts = line.split(sep, maxsplit=1)
        if len(parts) > 1:
            chunk = parts[1]
            sys.stdout.write(chunk)

if __name__ == '__main__':
    main()
