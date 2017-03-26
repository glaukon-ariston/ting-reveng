# -*- coding: utf-8 -*-
#
# Date: 03.05.2016
# Author: Glaukon Ariston
#
import os
import re
from defines import STATS_REPOSITORY
from misc import timestamp, run010EditorScript


RE_BOOK_DIR = re.compile(r'^\d{5}$', re.IGNORECASE)
RE_SCRIPT_FILE = re.compile(r'.+\.(s)$', re.IGNORECASE)
TITLE = 'analyseScripts'


def processAllBooks(repository):
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
        if bookId != '00024':
            run010EditorScript(TITLE, bookId)


def processSomeBooks(bookList):
    for bookId in bookList:
        run010EditorScript(TITLE, bookId)


def main():
    #run010EditorScript(TITLE, '00050')
    # 0x15 CALLID books
    #processSomeBooks(['00035','05129','05133','05134','05136','05151','05244'])
    # 0x14 CALL books
    #processSomeBooks(['00015', '00022', '00023', '00035', '00041', '05066', '05067', '05068', '05069', '05070', '05071', '05072', '05073', '05074', '05075', '05076', '05077', '05078', '05079', '05080', '05081', '05082', '05083', '05084', '05085', '05086', '05087', '05088', '05089', '05091', '05097', '05098', '05099', '05100', '05110', '05111', '05125', '05126', '05129', '05133', '05134', '05135', '05136', '05141', '05151', '05154', '05155', '05165', '05166', '05167', '05168', '05169', '05170', '05171', '05172', '05173', '05174', '05175', '05201', '05202', '05217', '05218', '05219', '05220', '05221', '05232', '05239', '05240', '05241', '05246', '05249', '05250', '05251', '05255', '05271', '05292', '05379',])
    processAllBooks(STATS_REPOSITORY)
    print '%s: Done' % (timestamp(),)


if __name__ == '__main__':
    main()
