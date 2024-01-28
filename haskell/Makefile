solve:
	cat inputs/Y$(Y)/$(D)/input.txt | stack run --profile -- 20$(Y) $(D)

example:
	cat inputs/Y$(Y)/$(D)/example.txt | stack run --profile -- 20$(Y) $(D)

exampleN:
	cat inputs/Y$(Y)/$(D)/example$(N).txt | stack run --profile -- 20$(Y) $(D)

new: download
	sed "s/YXX/Y$(Y)/g" ./src/DayXX.hs.template | sed "s/DayXX/Day$(D)/g" > ./src/Y$(Y)/Day$(D).hs

download:
	mkdir -p inputs/Y$(Y)/$(D)
	curl --cookie "session=$(SESSION)" https://adventofcode.com/20$(Y)/day/$(shell expr $(D) + 0)/input > inputs/Y$(Y)/$(D)/input.txt
	cat inputs/Y$(Y)/$(D)/input.txt
