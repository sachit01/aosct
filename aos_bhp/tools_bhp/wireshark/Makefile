all:
	@cat README.txt
	@echo "make gen : generate TCC dissector from XML"
	@echo "make gen-dmi : generate DMI dissector from XML"
	@echo "make gen-lcs : generate LCS dissector from XML"
	@echo "make test : test TCC dissector"
	@echo "make test-dmi : test DMI dissector"
	@echo "make test-lcs : test LCS dissector"

gen:
	python gendissect.py --xml ../TCCSim/Distribution/Software/XML --file=gen -u --name=tcc 2>&1 | tee gendissect.py.txt

gen-dmi:
	python gendissect.py --xml XML-DMI --file gendmi -u --autofields --name=dmi 2>&1 | tee gendissect-dmi.py.txt

gen-lcs:
	python gendissect.py --xml XML-LCS --file genlcs -u --autofields --name=lcs 2>&1 | tee gendissect-lcs.py.txt


test:
	tshark -X lua_script:gen-tcc-dissect.lua -r packet.test0/tsetup.pcap -V 2>&1 | tee test.packet.txt

test-dmi:
	tshark -X lua_script:gendmi-dmi-dissect.lua -r packet.test0/dmi-setup.pcap -V 2>&1 | tee dmi.packet.txt

test-lcs:
	tshark -X lua_script:genlcs-lcs-dissect.lua -r packet.test0/lcs-setup.pcap -V 2>&1 | tee lcs.packet.txt

conv:
	mkdir -p packet.test2.txt
	for f in `find packet.test2 -type f`; do \
		b=`basename $$f`; \
		cat $$f | xxd -p | tr -d '\n' > packet.test2.txt/$$b.txt; \
	done

convfile:
	echo "packets = [" > packet_test2.py
	for f in `find packet.test2.txt -type f`; do \
		p=`cat $$f`; \
		echo  " '$$p'," >> packet_test2.py; \
	done
	echo "];" >> packet_test2.py
