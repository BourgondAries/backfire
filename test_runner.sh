#!/bin/bash
rm -rf testOutput
mkdir testOutput

outExtention=""
vslProgramDir="vsl_programs"
correctDir="correctOutput"

warningsAssembler=0

singleStage=0
upToStage=3

cat > results.html <<XX
<html>
<head>
<title>Test Results</title>
<style>
iframe {
		border-style:solid;
		border-width:1px;
	   }
body {font-family:sans;}
button {
	background:none;
	border:none;
	}
button:hover{
	text-decoration:underline;
	}
</style>
<script>
myLoad = function(file, stage){
document.getElementById('diffframe').src = ("testOutput/" + file + "." + stage + ".html")
document.getElementById('inputframe').src = ("vsl_programs/" + file + ".vsl")
}
</script>
</head>

<body>
<div style="width:15%;float:left;font-family:sans;">
<h1>Test Report</h1>


The output was incorrect for the following files.
Click to see difference between your output and the correct output. <br>

XX


echo "Testing... found errors in the following files:"

for inputFile in `ls $vslProgramDir/*.vsl`; do
	#echo "Testing $inputFile ..."
	inputFileBase=`basename $inputFile .vsl`
	
	for i in {1..11}; do
		if [ $i -eq $singleStage ] || [ $i -le $upToStage ] ; then
			
			./bin/vslc -s $i < $inputFile > testOutput/$inputFileBase.s$i$outExtention
			diff -Nu correctOutput/$inputFileBase.s$i testOutput/$inputFileBase.s$i$outExtention > testOutput/$inputFileBase.s$i.diff
			
			if [ ! -s "testOutput/$inputFileBase.s$i.diff" ]; then
				#echo -e "\e[00;32mCorrect Stage $i \e[00m"
				rm testOutput/$inputFileBase.s$i*
			else
				./diff2html.sh < testOutput/$inputFileBase.s$i.diff > testOutput/$inputFileBase.s$i.html
				echo "<button onClick=myLoad(\"$inputFileBase\",\"s$i\")>$inputFileBase</button> <br>" >> results.html
				echo $inputFileBase

			fi
		fi
	done
	
	i=12
	if [ $singleStage -eq 12 ] || [ $upToStage -eq 12 ]; then
	
		./bin/vslc < $inputFile &> testOutput/$inputFileBase.s
		
                arm-linux-gnueabihf-gcc-4.8 -static testOutput/$inputFileBase.s -o testOutput/a.out 2> testOutput/$inputFileBase.asmError
			
	
		# Check if the assembly produces warnings/errors
		if [ ! -s "testOutput/$inputFileBase.asmError" ]; then
			rm testOutput/$inputFileBase.asmError
			removeAsm=1
		else
			removeAsm=0
			diff -Nu $correctDir/$inputFileBase.s12 testOutput/$inputFileBase.s > testOutput/$inputFileBase.s12.diff
			
			if [ ! -s "testOutput/$inputFileBase.s12.diff" ]; then
				rm testOutput/$inputFileBase.s12.diff
			else
			    ./diff2html.sh < testOutput/$inputFileBase.s$i.diff > testOutput/$inputFileBase.s$i.html
				echo "<button onClick=myLoad(\"$inputFileBase\",\"s$i\")>$inputFileBase</button> <br>" >> results.html
				echo $inputFileBase
			fi
		fi
	
		
		if [ $removeAsm -eq 1 ]; then
			qemu-arm ./testOutput/a.out 1 2 > testOutput/$inputFileBase.s12 
			
			
			
			diff -Nu $correctDir/$inputFileBase.s12 testOutput/$inputFileBase.s12 > testOutput/$inputFileBase.s12.diff
			
			if [ ! -s "testOutput/$inputFileBase.s12.diff" ]; then
				rm testOutput/$inputFileBase.s12*
			else
			    ./diff2html.sh < testOutput/$inputFileBase.s$i.diff > testOutput/$inputFileBase.s$i.html
				echo "<button onClick=myLoad(\"$inputFileBase\",\"s$i\")>$inputFileBase</button> <br>" >> results.html
				echo $inputFileBase

			fi
		fi
		
		rm -f testOutput/a.out
	fi	
done

echo "Done. For full report, see results.html"

cat >> results.html <<XX
</div>
<div style="width:40%;float:right;padding:5px;">
Input:<br>
<iframe height=95% width=100% id="inputframe" src=""></iframe>
</div>

<div style="width:40%;float:right;padding:5px;">
Diff: <br>
<iframe height=95% width=100% id="diffframe" src=""></iframe>
</div>
</body>

</html>
XX


