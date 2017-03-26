# -*- coding: utf-8 -*-
#
# Date: 04.05.2016
# Author: Glaukon Ariston
#
import os
import sys
import re
import codecs
from defines import STATS_REPOSITORY
from misc import timestamp


RE_BOOK_DIR = re.compile(r'^\d{5}$', re.IGNORECASE)
RE_DISASM_FILE = re.compile(r'.+\.(d)$', re.IGNORECASE)
TITLE = 'mergeDisasm'


def processBook(repository, bookId, footer):
	disasmFiles = []
	bookDir = os.path.join(repository, 'books', bookId)
	for root, dirnames, filenames in os.walk(bookDir):
		disasmFiles.extend(
			filename
			for filename in filenames if RE_DISASM_FILE.match(filename)
		)
		disasmFiles.sort()
		break
	if len(disasmFiles) == 0:
		return

	s = ''
	for f in disasmFiles:
		filepath = os.path.join(bookDir, f)
		with codecs.open(filepath, 'rb', 'utf-8') as f:
			s += f.read() + footer
	
	filepath = os.path.join(bookDir, '%s.dd' % bookId)
	print '%s: %s' % (timestamp(),filepath)
	with codecs.open(filepath, 'w', 'utf-8') as f:
		print >>f, s


def processAllBooks(repository, footer):
	bookIds = []
	booksDir = os.path.join(repository, 'books')
	for root, dirnames, filenames in os.walk(booksDir):
		bookIds.extend(
			os.path.relpath(os.path.join(root, dir), booksDir) 
			for dir in dirnames if RE_BOOK_DIR.match(dir)
		)
		break

	print '%s: About to process %s books from %s' % (TITLE, len(bookIds), booksDir)
	for bookId in bookIds:
		if bookId in ('00024'): continue
		processBook(repository, bookId, footer)


def main():
	#footer = '\n' if len(sys.argv) < 3 else sys.argv[2]
	footer = '\n'
	bookIds = []
	if len(sys.argv) > 1:
		bookIds = sys.argv[1:]

	if len(bookIds) == 0:
		processAllBooks(STATS_REPOSITORY, footer)
	else:
		for bookId in bookIds:
			processBook(STATS_REPOSITORY, bookId, footer)
	print '%s: Done' % (timestamp(),)


if __name__ == '__main__':
	main()
