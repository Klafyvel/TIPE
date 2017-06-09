#! /bin/zsh
corebuild -pkg str -pkg yojson -pkg sqlite3 experimentSpreadingRandom.native
corebuild -pkg str -pkg yojson -pkg sqlite3 experimentSpreadingDegree.native
corebuild -pkg str -pkg yojson -pkg sqlite3 experimentSpreadingBetween.native
corebuild -pkg str -pkg yojson -pkg sqlite3 experimentSpreadingAll.native
