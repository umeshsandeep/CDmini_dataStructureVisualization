import re
import ply.lex as lex
import ply.yacc as yacc
import turtle
from final import *
max_size=10
######################## variables required for parser ####################################
totVar = [] ######## stores all variables declared as stack and queue
Queueint = [] ###### stores Queue variable names decalred as int
Queuestring = []
Queuefloat = []
Stackint =[]    ####### stores stack variable names declared as int
Stackstring =[]
Stackfloat=[]
LinkedList = []
CouDic = {}  ##### stores length of queue or stack
idsDic = {}
idsDic1 = {}
idsDic2 = {}
patternfloat = re.compile("([0-9]+)[.]([0-9]+)")
patternint = re.compile("[0-9]+")
patternchar = re.compile("([a-zA-Z]|[_])([0-9a-zA-Z]|(_))+")

data=''
tokens = (
#	'DSQ',
#	'DSS',
#	'DTI',
#	'DTF',
#	'DTS',
#	'OPER',
	'LPARS',
	'RPARS',
	'NUMBER',
	'FLOAT',
	'PLUS',
	'MINUS',
	'MULT',
	'DIVI',
	'SEMICO',
	'EQUAL',
	'LE',
	'GE',
	'LL',
	'GG',
	'EE',
#	'STRING',
	'COMMA',
	'INSER',
	'DELET',
	'ERASE',
    'ID',
    'LSBS',
    'RSBS',
	)
t_EE = r'=='
t_GE = r'>='
t_LE = r'<='
t_LL = r'<'
t_GG = r'>'
#t_DSQ = r'Queue[\s]*'
#t_DSS = r'Stack[\s]*'
#t_DTI = r'int[\s]*'
#t_DTF = r'float[\s]*'
#t_DTS = r'string[\s]*'
t_LSBS  = r'{'
t_RSBS  = r'}'
t_COMMA = r','
t_INSER = r'(.insert)'
t_DELET = r'(.delete)'
t_ERASE = r'(.erase)'
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_MULT   = r'\*'
t_DIVI  = r'/'
t_LPARS  = r'\('
t_RPARS  = r'\)'
t_SEMICO = r';'
t_EQUAL = r'='
#t_STRING = r'[a-zA-Z]+[a-zA-Z0-9]*'

# A regular expression rule with some action code
def t_FLOAT(t):
	r'[\d]+[.][\d]+'
	t.value = float(t.value)
	return t
def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t



# Define a rule so we can track line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# A string containing ignored characters (spaces and tabs)
t_ignore  = ' \t'

# Error handling rule
def t_error(t):
    print "Illegal character '%s'" % t.value[0]
    t.lexer.skip(1)

RESERVED = {
	'Queue' : 'DSQ',
	'Stack' : 'DSS',
	'linkedlist':'DSL',
	'int'   : 'DTI',
	'float'	: 'DTF',
	'string':	'DTS',
	'if' : 'IF',
	'else' : 'ELSE',
	'for' : 'FOR',
	'in' : 'IN',
	'range' : 'RANGE',
}

tokens = tokens + tuple(RESERVED.values())

def t_ID(t):
	r'[a-zA-Z_][a-zA-Z0-9_]*'
	t.type = RESERVED.get(t.value, 'ID')
	return t

t_ignore_COMMENT = r'\#.*'

# Build the lexer



#################################  parsing ####################################################
def match(k):
	if k in idsDic:
		if patternfloat.match(str(idsDic[k])):
			return "float"
		elif patternint.match(str(idsDic[k])):
			return "int"
		
		else:
			return "string"
	else:
		if patternfloat.match(str(k)):
			return "float"
		elif patternint.match(str(k)):
			return "int"
		
		else:
			return "string"

def p_statement(p) :
	'''
	statement : start
	'''
	#if p[1]:
	#	print p[1]
def p_start(p):

	'''
	start : declaration start

	      | operations start

		  | expression SEMICO start

		  | IF condition SEMICO start

		  | IF  conditione SEMICO start

		  | FOR conditionw SEMICO start

		  | empty
	'''
	if p[1] == 'if':
		p[0] = [p[1],p[2]]
	else:
		p[0] = p[1]
	#print p[0]
def p_s(p):
	#print p ,"umesh"
	if (p[0] in Queueint) or (p[0] in Queuefloat) or (p[0] in Queuestring):
		queuePush(p[0],p[2])
		CouDic[p[0]] = CouDic[p[0]]+1
	elif (p[0] in Stackint) or (p[0] in Stackfloat) or (p[0] in Stackstring):
		stackPush(p[0],p[2])
		CouDic[p[0]] = CouDic[p[0]]+1
def p_conw(p):
	'''
	conditionw : LPARS expressionz RPARS LSBS operations RSBS
	'''
	k = p[5][3]
	k1 = p[2][0]
	#print "k",k,k1
	for p[2][0] in range(1,p[2][1]):
		if k in idsDic and k!=k1:
			p[5][2] = idsDic[k]
		if k == k1:
			p[5][2] = p[2][0]
		#else:
		#	p[5][2] = p[2][0]
		p_s(p[5])

def p_expz(p):
	'''
	expressionz : ID IN RANGE NUMBER
	'''
	idsDic[p[1]] = 0
	p[0] = [p[1] , p[4]]
def p_empty(p):

	'''
	empty :
	'''
	pass
	#p[0] = 'Empty'
def p_cone(p):
	'''
	conditione : LPARS expression RPARS LSBS start1 RSBS SEMICO ELSE LSBS start2 RSBS

	'''
	if int(p[2]) > 0:
	#	print "yes"
		idsDic.update(idsDic1)
		idsDic1.clear()
		idsDic2.clear()
	else:
		idsDic.update(idsDic2)
		idsDic2.clear()
		idsDic1.clear()

	p[0] = [p[2],p[5]]
def p_con(p):
	'''
	condition : LPARS expression RPARS LSBS start1 RSBS
				| LPARS expression RPARS LSBS IF conditionz SEMICO RSBS

	'''
	if int(p[2]) > 0 and len(p) == 7:
	#	print "yes"
		idsDic.update(idsDic1)
		idsDic1.clear()
		idsDic2.clear()
		p[0] = [p[2],p[5]]
	else:
	#	print p[2],p[6]
		if int(p[2])>0 and int(p[6][1])>0:
	#		print "yes"
			idsDic.update(idsDic1)
			idsDic1.clear()
			idsDic2.clear()
			p[0] = [p[2],p[6][1]]
def p_mn(p):
	'''
	conditionz : LPARS expression RPARS LSBS start1 RSBS
	'''
	if int(p[2]) > 0 and len(p) == 7:
	#	print "yes"
		p[0] = [p[2],p[5]]

def p_exp(p):
	'''
	start1 : expressiona SEMICO start1

		   | empty
	'''
	p[0]=p[1]
def p_expp1(p):
	'''
	start2 : expressionb SEMICO start2
		   | empty
	'''
def p_Dec(p):
	'''
	declaration : ds dt ID SEMICO
				| ds ID SEMICO
	'''
	if len(p) == 4:
		if p[2] in totVar :
			print "variable ",p[2],"re declared"
		if p[1] == "linkedlist":
			totVar.append(p[2])
			CouDic[p[2]] = 0                  ##### call create linked list here p[2] has variable name ################
			LinkedList.append(p[2])
			createLinked(p[2])
		p[0] = [p[1],p[2]]
	else:
		if p[3] in totVar:
			print "variable",p[3],"redeclared" ############# error here ##############
		if p[1] =='Queue':
			totVar.append(p[3])
			CouDic[p[3]] = 0
			createQueue(p[3])
			if p[2] == 'int':
				Queueint.append(p[3])
			elif p[2] == 'float':
				Queuefloat.append(p[3])
			elif p[2] == 'string':
				Queuestring.append(p[3])
		elif p[1] == 'Stack':
			createStack(p[3])
			totVar.append(p[3])
			CouDic[p[3]] = 0
			if p[2] == 'int':
				Stackint.append(p[3])
			elif p[2] == 'float':
				Stackfloat.append(p[3])
			elif p[2] == 'string':
				Stackstring.append(p[3])
		p[0] = [p[1],p[2],p[3]]
def p_operations(p):
	'''
	operations : ID oper  SEMICO
	'''
	t=p[2][0]
	if p[2][0] == '.erase' : #TODO changed
		if p[1] not in totVar:
			print "trying to delete not declared variable"
		elif p[1] in Queueint:
			Queueint.remove(p[1])
			eraseQueue(p[1])
			del CouDic[p[1]]
		elif p[1] in Queuefloat:
			Queuefloat.remove(p[1])
			eraseQueue(p[1])
			del CouDic[p[1]]
		elif p[1] in Queuestring:
			Queuestring.remove(p[1])
			eraseQueue(p[1])
			del CouDic[p[1]]
		elif p[1] in Stackint:
			eraseStack(p[1])
			Stackint.remove(p[1])
			del CouDic[p[1]]
		elif p[1] in Stackfloat:
			eraseStack(p[1])
			Stackfloat.remove(p[1])
			del CouDic[p[1]]
		elif p[1] in Stackstring:
			eraseStack(p[1])
			Stackstring.remove(p[1])
			del CouDic[p[1]]
		elif p[1] in LinkedList:
			eraseLinked(p[1])							############## call erase linkedlist here p[1] has variable name###
			LinkedList.remove(p[1])
			del CouDic[p[1]]


	elif p[2][0] == '.insert':
		t=p[2][1]
		#print match(str(p[2][1])) , p[2][1]
		if p[2][1] in idsDic:
			p[2][1] = idsDic[p[2][1]]
	#	print p[2],p[1] #TODO changed
		if p[1] not in totVar:          ##################### error here ################
			print p[1],"variable not declared"
		elif p[1] in LinkedList:
			if CouDic[p[1]] == max_size:
				print "Max Size of Linked list reached"
			else :
				insertNode(p[2][1],p[2][2])
				CouDic[p[1]] = CouDic[p[1]]+1
				                              ##### call insert linked list here p[2][1] = position , p[2][2] = value #####
		elif p[1] in Queueint:
			if CouDic[p[1]] == max_size:
				print "MAX Size of Queue Reached"
			if match(str(p[2][1]))=='int':
			#f.write(k1 + ".append("+ k3 +")"+"\n")
				queuePush(p[1],p[2][1])
				CouDic[p[1]] = CouDic[p[1]]+1
			else:
				#f.write("print "+ '"' +" datatype mismatch" +'"' +"\n"+"sys.exit()\n")
				print str(p[1])+" and "+str(p[2][1])+ " datatype  mismatch\n"
		elif p[1] in Queuefloat:
			if CouDic[p[1]] == max_size:
				print "MAX Size of Queue Reached"
			if match(str(p[2][1]))=='float':
				#f.write(k1 + ".append("+ k3 +")"+"\n")
				#cou = CouDic[k1]
				#cou = cou + 1
				queuePush(p[1],p[2][1])
				CouDic[p[1]] = CouDic[p[1]]+1
			else:
				#f.write("print "+ '"' +"datatype mismatch" +'"' +"\n"+"sys.exit()\n")
				print str(p[1])+" and "+str(p[2][1])+ " datatype mismatch\n"
				#sys.exit()
		elif p[1] in Queuestring:
			if CouDic[p[1]] == max_size:
				print "MAX Size of Queue Reached"
			if match(str(p[2][1]))=='string':
				queuePush(p[1],p[2][1])
				CouDic[p[1]] = CouDic[p[1]]+1
			else :
				print str(p[1])+ " and "+str(p[2][1])+" datatype mismatch\n"
		elif p[1] in Stackint:
			if CouDic[p[1]] == max_size:
				print "MAX Size of Stack Reached"
			if match(str(p[2][1]))=='int':
			#f.write(k1 + ".append("+ k3 +")"+"\n")
				stackPush(p[1],p[2][1])
				CouDic[p[1]] = CouDic[p[1]]+1
			else:
				#f.write("print "+ '"' +" datatype mismatch" +'"' +"\n"+"sys.exit()\n")
				print str(p[1])+" and "+str(p[2][1])+ " datatype  mismatch\n"
		elif p[1] in Stackfloat:
			if CouDic[p[1]] == max_size:
				print "MAX Size of Stack Reached"
			if match(str(p[2][1]))=='float':
				#f.write(k1 + ".append("+ k3 +")"+"\n")
				#cou = CouDic[k1]
				#cou = cou + 1
				stackPush(p[1],p[2][1])
				CouDic[p[1]] = CouDic[p[1]]+1
			else:
				#f.write("print "+ '"' +"datatype mismatch" +'"' +"\n"+"sys.exit()\n")
				print str(p[1])+" and "+str(p[2][1])+ " datatype mismatch\n"
				#sys.exit()
		elif p[1] in Stackstring:
			if CouDic[p[1]] == max_size:
				print "MAX Size of Stack Reached"
			if match(str(p[2][1]))=='string':
				stackPush(p[1],p[2][1])
				CouDic[p[1]] = CouDic[p[1]]+1
			else :
				print str(p[1])+ " and "+str(p[2][1])+" datatype mismatch\n"
	elif p[2][0] == '.delete':  #TODO changed
		if p[1] not in totVar:
			print p[1],"variable not declared"
		elif ((p[1] in Queueint) or (p[1] in Queuefloat) or (p[1] in Queuestring)) :
			if CouDic[p[1]] == 0:
				print "Min Size of queue Reached "
			else:
				queuePop(p[1])
 				CouDic[p[1]] = CouDic[p[1]]-1
		elif (p[1] in Stackint) or (p[1] in Stackfloat) or (p[1] in Stackstring) :
			if CouDic[p[1]] == 0:
				print "Min SIze of Stack Reached"
			else:
				stackPop(p[1])
				CouDic[p[1]] = CouDic[p[1]]-1
		elif p[1] in LinkedList:
			if CouDic[p[1]] == 0:
				print "Min Size of linkedlist Reached"
			else:
				deleteNode(p[2][1])		######################## call delete linklist here p[2][1] has position #####
				CouDic[p[1]] = CouDic[p[1]]-1

	p[0] = [p[1],p[2][0],p[2][1],t]
def p_oper(p):
	'''
	oper : INSER LPARS NUMBER RPARS

		| INSER LPARS ID RPARS

		| INSER LPARS NUMBER COMMA NUMBER RPARS

		| INSER LPARS NUMBER COMMA ID RPARS

		| DELET LPARS RPARS

		| DELET LPARS NUMBER RPARS

		| ERASE LPARS RPARS

		| INSER LPARS FLOAT RPARS

		| INSER LPARS NUMBER COMMA FLOAT RPARS

		| DELET LPARS FLOAT RPARS
	'''
	if len(p) == 7:
		p[0] = [p[1],p[3],p[5]]
	else:
		p[0] = [p[1],p[3]]

def p_ds(p):
	'''
	ds : DSQ

		| DSS
		| DSL
	'''
	p[0] = p[1]

def p_dt(p):
	'''
	dt : DTI

		| DTF

		| DTS
	'''
	p[0] = p[1]
def p_expression_plus(p):
    '''
    expression : expression PLUS term
    '''
    #print p[1] , p[3]
    p[0] = p[1] + p[3]
def p_expression_minus(p):
    '''
    expression : expression MINUS term
    '''
    p[0] = p[1] - p[3]

def p_expression_term(p):
    '''
    expression : term
    '''
    p[0] = p[1]

def p_term_times(p):
    '''
    term : term MULT factor
    '''
    p[0] = p[1] * p[3]

def p_term_div(p):
    '''
    term : term DIVI factor
    '''
    p[0] = p[1] / p[3]

def p_term_factor(p):
    '''
    term : factor

    '''
    p[0] = p[1]
def p_fact_nu(p):
	'''
	factor : ID
	'''
	if p[1] in idsDic:
		p[0] = idsDic[p[1]]
	else :
		p[0] = p[1]
def p_factor_num(p):
    '''
    factor : NUMBER

    		| FLOAT

    '''
    p[0] = p[1]
def p_factor_equal(p):
	'''
	term : ID EQUAL factor
		| ID EQUAL factor PLUS factor

		| factor EE factor

		| factor GG factor
		| factor LL factor
		| factor LE factor

		| factor GE factor


	'''
	#print p[2]
	if (len(p) == 4) and (p[2] == '='):

		if (match(str(p[3])) == 'int') or (match(str(p[3])) == 'float'):
			p[0] = p[3]
			idsDic[p[1]] = p[3]
		else:
				if p[3] in idsDic:
					idsDic[p[1]] = idsDic[p[3]]
					p[0] = idsDic[p[3]]
				else:
					print "Error"
	elif len(p) == 4:
		p[0] = 0
		#print p[3],p[1]
		if ((match(str(p[3])) == 'int') or (match(str(p[3])) == 'float')) and ((match(str(p[1])) == 'int') or (match(str(p[1])) == 'float')):
			# print p[1],p[3]
			if p[2] == '==':
				if p[1] == p[3]:
					p[0] = 1
			elif p[2] == '>=':
				if p[1] >= p[3]:
					p[0] = 1
			elif p[2] == '<=':
				if p[1] <= p[3]:
					p[0]= 1
			elif p[2] == '<':
				if p[1] < p[3]:
					p[0]= 1
			elif p[2] == '>':
				if p[1] > p[3]:
					p[0]=1
			# print p[0],"hello"
		elif (match(str(p[3])) == 'int') or (match(str(p[3])) == 'float'):

			if p[1] in idsDic:
				p[1] = idsDic[p[1]]
		#		print p[1],p[2],p[3]

				if p[2] == '==':
					if p[1] == p[3]:
						p[0] = 1
				elif p[2] == '>=':
					if p[1] >= p[3]:
						p[0] = 1
				elif p[2] == '<=':
					if p[1] <= p[3]:
						p[0]= 1
				elif p[2] == '<':
					if p[1] < p[3]:
						p[0]= 1
				elif p[2] == '>':
					if p[1] > p[3]:
						p[0]=1
			else:
				print "Error"
		elif  (match(str(p[1])) == 'int') or (match(str(p[1])) == 'float'):
			if p[3] in idsDic:
				p[3] = idsDic[p[3]]
				if p[2] == '==':
					if p[1] == p[3]:
						p[0] = 1
				elif p[2] == '>=':
					if p[1] >= p[3]:
						p[0] = 1
				elif p[2] == '<=':
					if p[1] <= p[3]:
						p[0]= 1
				elif p[2] == '<':
					if p[1] < p[3]:
						p[0]= 1
				elif p[2] == '>':
					if p[1] > p[3]:
						p[0]=1
			else:
				print "Error"
		else:

			if (p[1] in idsDic) and (p[3] in idsDic):
				p[1] = idsDic[p[1]]
				p[3] = idsDic[p[3]]
				if p[2] == '==':
					if p[1] == p[3]:
						p[0] = 1
				elif p[2] == '>=':
					if p[1] >= p[3]:
						p[0] = 1
				elif p[2] == '<=':
					if p[1] <= p[3]:
						p[0]= 1
				elif p[2] == '<':
					if p[1] < p[3]:
						p[0]= 1
				elif p[2] == '>':
					if p[1] > p[3]:
						p[0]=1
			else :
				if p[2] == '==':
					if p[1] == p[3]:
						p[0] = 1
				elif p[2] == '>=':
					if p[1] >= p[3]:
						p[0] = 1
				elif p[2] == '<=':
					if p[1] <= p[3]:
						p[0]= 1
				elif p[2] == '<':
					if p[1] < p[3]:
						p[0]= 1
				elif p[2] == '>':
					if p[1] > p[3]:
						p[0]=1


	else:
		if (match(str(p[3])) == 'int') or (match(str(p[3])) == 'float'):
				if (match(str(p[5])) == 'int') or (match(str(p[5])) == 'float'):
					idsDic[p[1]] = p[3] + p[5]
					p[0] = p[3]+p[5]
				else :
					if p[5] in idsDic:
						idsDic[p[1]] = p[3] + idsDic[p[5]]
						p[0] = idsDic[p[1]]

					else:
						print "Error at",p[5]
		else :
				if (match(str(p[5])) == 'int') or (match(str(p[5])) == 'float') :
					if p[3] in idsDic:
						idsDic[p[1]] = idsDic[p[3]] + p[5]
						p[0] = p[3]+p[5]
				else :
					if (p[5] in idsDic) and (p[3] in idsDic):
						idsDic[p[1]] = idsDic[p[3]] + idsDic[p[5]]
						p[0] = idsDic[p[1]]

					else:
						print "error at",p[3],p[5]
						'''	try:

					assert idsDic[p[3]].isdigit()
					p[0] = idsDic[p[3]]
				except LookupError:
					print("Undefined id '%s'" % p[3])
				except AssertionError:
					print 'Expected an integer but got something else...!!'  '''
	#p[0] = p[3]
def p_factor_expr(p):
    '''
    factor : LPARS expression RPARS SEMICO
    '''
    p[0] = p[2]

##########################################################################################



def p_expression_plus1(p):
    '''
    expressiona : expressiona PLUS terma
    '''
    #print p[1] , p[3]
    p[0] = p[1] + p[3]
def p_expression_minus1(p):
    '''
    expressiona : expressiona MINUS terma
    '''
    p[0] = p[1] - p[3]

def p_expression_terma(p):
    '''
    expressiona : terma
    '''
    p[0] = p[1]

def p_term_times1(p):
    '''
    terma : terma MULT factora
    '''
    p[0] = p[1] * p[3]

def p_term_div1(p):
    '''
    terma : terma DIVI factora
    '''
    p[0] = p[1] / p[3]

def p_term_factora(p):
    '''
    terma : factora

    '''
    p[0] = p[1]
def p_fact_nu1(p):
	'''
	factora : ID
	'''
	if p[1] in idsDic:
		p[0] = idsDic[p[1]]
	else :
		p[0] = p[1]
def p_factor_num1(p):
    '''
    factora : NUMBER
            | FLOAT

    '''
    p[0] = p[1]
def p_factor_equal1(p):
	'''
	terma : ID EQUAL factora
		| ID EQUAL factora PLUS factora

	'''
	if len(p) == 4:
		if (match(str(p[3])) == 'int') or (match(str(p[3])) == 'float'):
			p[0] = p[3]
			idsDic1[p[1]] = p[3]
		else:
				if p[3] in idsDic:
					idsDic1[p[1]] = idsDic[p[3]]
					p[0] = idsDic[p[3]]
				else:
					print "Error"
	else:
		if (match(str(p[3])) == 'int') or (match(str(p[3])) == 'float'):
				if (match(str(p[5])) == 'int') or (match(str(p[5])) == 'float'):
					idsDic1[p[1]] = p[3] + p[5]
					p[0] = p[3]+p[5]
				else :
					if p[5] in idsDic:
						idsDic1[p[1]] = p[3] + idsDic[p[5]]
						p[0] = idsDic1[p[1]]

					else:
						print "Error at",p[5]
		else :
				if (match(str(p[5])) == 'int') or (match(str(p[5])) == 'float') :
					if p[3] in idsDic:
						idsDic1[p[1]] = idsDic[p[3]] + p[5]
						p[0] = p[3]+p[5]
				else :
					if (p[5] in idsDic) and (p[3] in idsDic):
						idsDic1[p[1]] = idsDic[p[3]] + idsDic[p[5]]
						p[0] = idsDic1[p[1]]

					else:
						print "Error at",p[3],p[5]
						'''	try:

					assert idsDic[p[3]].isdigit()
					p[0] = idsDic[p[3]]
				except LookupError:
					print("Undefined id '%s'" % p[3])
				except AssertionError:
					print 'Expected an integer but got something else...!!'  '''
	#p[0] = p[3]
def p_factor_expr1(p):
    '''
    factora : LPARS expressiona RPARS SEMICO
    '''
    p[0] = p[2]


##########################################################################################





def p_expression_plus2(p):
    '''
    expressionb : expressionb PLUS termb
    '''
    #print p[1] , p[3]
    p[0] = p[1] + p[3]
def p_expression_minus2(p):
    '''
    expressionb : expressionb MINUS termb
    '''
    p[0] = p[1] - p[3]

def p_expression_term2(p):
    '''
    expressionb : termb
    '''
    p[0] = p[1]

def p_term_times2(p):
    '''
    termb : termb MULT factorb
    '''
    p[0] = p[1] * p[3]

def p_term_div2(p):
    '''
    termb : termb DIVI factorb
    '''
    p[0] = p[1] / p[3]

def p_term_factor2(p):
    '''
    termb : factorb

    '''
    p[0] = p[1]
def p_fact_nu2(p):
	'''
	factorb : ID
	'''
	if p[1] in idsDic:
		p[0] = idsDic[p[1]]
	else :
		p[0] = p[1]
def p_factor_num2(p):
    '''
    factorb : NUMBER

            | FLOAT
    '''
    p[0] = p[1]
def p_factor_equal2(p):
	'''
	termb : ID EQUAL factorb
		| ID EQUAL factorb PLUS factorb

	'''
	if len(p) == 4:
		if (match(str(p[3])) == 'int') or (match(str(p[3])) == 'float'):
			p[0] = p[3]
			idsDic2[p[1]] = p[3]
		else:
				if p[3] in idsDic:
					idsDic2[p[1]] = idsDic[p[3]]
					p[0] = idsDic[p[3]]
				else:
					print "Error"
	else:
		if (match(str(p[3])) == 'int') or (match(str(p[3])) == 'float'):
				if (match(str(p[5])) == 'int') or (match(str(p[5])) == 'float'):
					idsDic2[p[1]] = p[3] + p[5]
					p[0] = p[3]+p[5]
				else :
					if p[5] in idsDic:
						idsDic2[p[1]] = p[3] + idsDic[p[5]]
						p[0] = idsDic2[p[1]]

					else:
						print "Error at",p[5]
		else :
				if (match(str(p[5])) == 'int') or (match(str(p[5])) == 'float') :
					if p[3] in idsDic:
						idsDic2[p[1]] = idsDic[p[3]] + p[5]
						p[0] = p[3]+p[5]
				else :
					if (p[5] in idsDic) and (p[3] in idsDic):
						idsDic2[p[1]] = idsDic[p[3]] + idsDic[p[5]]
						p[0] = idsDic2[p[1]]

					else:
						print "Error at",p[3],p[5]
						'''	try:

					assert idsDic[p[3]].isdigit()
					p[0] = idsDic[p[3]]
				except LookupError:
					print("Undefined id '%s'" % p[3])
				except AssertionError:
					print 'Expected an integer but got something else...!!'  '''
	#p[0] = p[3]
def p_factor_expr2(p):
    '''
    factorb : LPARS expressionb RPARS SEMICO
    '''
    p[0] = p[2]




##########################################################################################

def p_error(p):

	print "Syntax Error at " ,p

turtle.setup( width = 1200, height = 1100, startx = None, starty = None)
s=turtle.Shape("compound")
poly1=((0,0),(50,0),(50,50),(0,50))
s.addcomponent(poly1,"white","black")
turtle.register_shape("myshape",s)
codefile=open('code.txt','r+')
data1=codefile.read()
data=data1
#print data1
initGui()

lexer = lex.lex()

lexer.input(data)

# Tokenize
print "########################################################################"
print "..........................Tokens Recognized:...........................\n"
while True:
    tok = lexer.token()
    if not tok: break      # No more input
    print tok
print "#######################################################################"
print ".......................Parsing ................................\n"
print "\n"
print ".....................Errors Found:.............................\n"
print "\n"


yacc.yacc()
yacc.parse(data)
print "\n"
print "################### Variables stored ###################################"
print "Check the Variables (whether the loops executed correctly :) ",idsDic
turtle.mainloop()
