solve:
	cat inputs/Y$(Y)/$(D)/input.txt | stack run --profile -- 20$(Y) $(D)

example:
	cat inputs/Y$(Y)/$(D)/example.txt | stack run --profile -- 20$(Y) $(D)

exampleN:
	cat inputs/Y$(Y)/$(D)/example$(N).txt | stack run --profile -- 20$(Y) $(D)

new:
	sed "s/YXX/Y$(Y)/g" ./src/DayXX.hs.template | sed "s/DayXX/Day$(D)/g" > ./src/Y$(Y)/Day$(D).hs
