all: bnfc ParLatte.hs LexLatte.hs

LexLatte.hs: LexLatte.x
	alex -g $<

ParLatte.hs: ParLatte.y
	happy -gca $<


bnfc: Latte.cf
	bnfc $<
