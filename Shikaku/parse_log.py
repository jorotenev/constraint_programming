# really quick and dirty file to parse the logs and generate a Latex table.

import sys,os,re
from tabulate import tabulate
from decimal import Decimal


file_prefix = 'output_'
logDir = sys.argv[1] + '/'
# in the prolog program, if there's a stage which is just to generate 
# the input, before starting the actual search, then this should be true
# if there isn't just pass some second argumend. God this is ugly.
hasGenerationStage = True if len(sys.argv) == 3 else False
timeoutPattern = re.compile("^Timeout is (\d+)$",re.MULTILINE)
generationPattern = re.compile('^% (\S+) inferences, .+ in (\S+) seconds')
searchPattern = re.compile("^% (\d+\.\d+|\d+) seconds cpu time for (\S+) inferences")

def getLineStartingWith(contentList, lineprefix):
	return [c for c in contentList if c.startswith(lineprefix)][0]

def getTimeout(content):
	try:
		line = getLineStartingWith(content,'Timeout is ')
		p = timeoutPattern
		a = re.match(p, line).groups()
		return a[0]
	except Exception as ex:
		pass

def readFile(file):
	with open(logDir+file, 'r') as logFile:
	    data=logFile.readlines()
	    return [s.strip() for s in data]

def problemGenerationInferencesAndTime(content):
	try:
		line = content[3]
		assert 'inferences' in line and 'CPU' in line

		p = generationPattern
		a = re.match(p,line).groups()
		return a[1],a[0]
	except:
		return "TIMED-OUT", "TIMED-OUT"

def getProblemName(content):
	return content[0].replace('begin-- ', '')

def getSearchStatistics(content, genSecs):
	try:
		line = content[6]
		p = searchPattern
		seconds,inferences = re.match(p, line).groups()
		assert seconds and inferences
		seconds = Decimal(seconds)
		genSecs = Decimal(genSecs)
		if hasGenerationStage:
			seconds = seconds - genSecs

		return seconds, inferences

	except Exception as ex:
		# print(ex)
		return "TIMED-OUT", "TIMED-OUT"

def runBaby(files, logDir):
	files.sort()
	lines = []
	timeout = -1
	for file in files:
		logContent = readFile(file)
		problemName = getProblemName(logContent)
		# print(problemName)

		timeout = getTimeout(logContent) 
		timeout = timeout if timeout else -1
		# print(timeout)
		genSecs, genInf = problemGenerationInferencesAndTime(logContent)
		# print(genSecs, genInf)

		searchSecs,searchInf = getSearchStatistics(logContent,genSecs)
		# print(searchSecs,searchInf)
		line = [problemName, searchSecs]
		if hasGenerationStage:
			line.append(genSecs)
		lines.append(line)
	lines.sort() # based on problem name
	if hasGenerationStage:
		headers = ['Name', 'Search Time', 'Problem generation time']
	else:
		headers = ['Name', 'Problem generation + Search Time']
	table =  tabulate(lines, headers=headers, tablefmt='latex_booktabs')
	iteration = logDir.replace('log', "").replace('/','')
	caption ='\caption{%s}' % 'Iteration %s. Time is in seconds. Maximum run-time limit is %s seconds. '%(iteration, timeout)+'\n'
	label = '\label{tab:%s}'%iteration+'\n'
	table = '\\begin{table}\n' +caption+ label+table + '\end{table}'
	with open( logDir+'RESULT.latex','w') as save_file:
		print(table, file=save_file) 	
		# print(table) 	



if __name__ == '__main__':
	files = [f for f in os.listdir(logDir) if f.startswith(file_prefix)]
	runBaby(files, logDir)
	print("Check %s for output"%logDir)