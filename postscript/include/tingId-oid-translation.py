# -*- coding: utf-8 -*-
"""
Date: 16.05.2016
Author: Glaukon Ariston
Abstract:
	The tingId, the number that gets mentioned in OUF files and scripts, is not what
	gets encoded in the dot pattern on the paper. Instead, the tingId gets translated
	into another number, the OID, and that number is encoded in the dot pattern on 
	the paper.
	It seems the translation algorithm is still unknown, but what is known is that it
	consists of interleaving of the two number sequances: one starting at zero and the 
	other at 4717.

	Data and code from the Martin-Dames/Tingeltangel project, see:
		https://github.com/Martin-Dames/Tingeltangel/blob/master/src/main/resources/id_trans.data
		https://github.com/Martin-Dames/Tingeltangel/blob/master/src/main/java/tingeltangel/core/Translator.java
"""
import os
import re
import codecs


def generateOidMap():
	with codecs.open('id_trans.data', 'r', 'UTF-8') as f:
		oid2tingId = []
		lastOid = -1
		for line in f.readlines():
			line = line.strip()
			if len(line) == 0: continue
			if line.startswith('#'): continue
			if line.startswith('set '):
				setKeyword, oid, seq1, seq2 = (a.strip() for a in line.split(u' '))
				oid, seq1, seq2 = (int(a) for a in (oid, seq1, seq2))
				seq2 -= 1
				print oid, seq1, seq2
			else:
				newOid = int(line) 
				print newOid
				if lastOid >= newOid:
					raise Exception('Unexpected condition: !(lastOid >= newOid)')
				lastOid = newOid;
				
				for i in range(oid, newOid):
					seq2 += 1
					oid2tingId.append((i, seq2))

				seq1 += 1
				oid2tingId.append((newOid, seq1))
				oid = newOid + 1
		return oid2tingId


def main():
	oid2tingId = generateOidMap()
	with codecs.open('map_oid_tingId.txt', 'w', 'UTF-8') as f:
		print >> f, 'oid;tingid'
		for oid, tingId in oid2tingId:
			print >> f, '%d;%d' % (oid, tingId)

	with codecs.open('map_tingId_oid.txt', 'w', 'UTF-8') as f:
		print >> f, 'tingid;oid'
		for oid, tingId in sorted(oid2tingId, key=lambda a: a[1]):
			print >> f, '%d;%d' % (tingId, oid)

	with codecs.open('map_tingId_oid.ps', 'w', 'UTF-8') as f:
		tingid2oid = sorted(oid2tingId, key=lambda a: a[1])
		_, startTingId = tingid2oid[0]
		lastTingId = startTingId-1
		print >> f, '['
		for oid, tingId in tingid2oid:
			for i in range(lastTingId+1, tingId):
				print >> f, '%d' % (-1)
			print >> f, '%d' % (oid)
			lastTingId = tingId
		print >> f, ']'
		print >> f, '/mapTingidOid exch def'
		print >> f, '/mapBaseTingid %d def' % (startTingId,)


if __name__ == '__main__':
	main()


