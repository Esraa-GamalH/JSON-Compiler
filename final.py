import ply.lex as lex

reserved = {
    'READ': 'READ',
    'read': 'read',
    'WRITE': 'WRITE',
    'write': 'write',
    'SET': 'SET',
    'set': 'set',
    'IF': 'IF',
    'if': 'if',
    'THEN': 'THEN',
    'then': 'then',
    'ELSE': 'ELSE',
    'else': 'else',
    'ENDIF': 'ENDIF',
    'endif': 'endif',
    'while': 'while',
    'WHILE': 'WHILE',
    'DO': 'DO',
    'do': 'do',
    'ENDWHILE': 'ENDWHILE',
    'endwhile': 'endwhile',
    'UNTIL': 'UNTIL',
    'until': 'until',
    'ENDUNTIL': 'ENDUNTIL',
    'enduntil': 'enduntil',
    'CALL': 'CALL',
    'call': 'call',
    'double': 'Datatype',
    'DOUBLE': 'Datatype',
    'float': 'Datatype',
    'FLOAT': 'Datatype',
    'real': 'Datatype',
    'REAL': 'Datatype',
    'int': 'Datatype',
    'INT': 'Datatype',
    'integer': 'Datatype',
    'INTEGER': 'Datatype',
    'str': 'Datatype',
    'STR': 'Datatype',
    'char': 'Datatype',
    'CHAR': 'Datatype',
    'PARAMETERS': 'PARAMETERS',
    'parameters': 'parameters',
    'DECLARE': 'DECLARE',
    'declare': 'declare',
    'BEGIN': 'BEGIN',
    'begin': 'begin',
    'end': 'end',
    'END': 'END',
    'PROCEDURE': 'PROCEDURE',
    'procedure': 'procedure',
    'assign' : 'assign',
    'function' :'function',
}

tokens = [
    'ID',
    'FLOAT',
    'NUMBER',
    'SEMICOLON',
    'LPAREN',
    'RPAREN',
    'string',
    'COMMA',
    'ARITHMETIC_OP',
    'RELATIONAL_OP',
    'Datatype',
    'LCURL',
    'RCURL',
] + list(reserved.values())

t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LCURL = r'\{'
t_RCURL = r'\}'

t_SEMICOLON = r';'
t_COMMA = r','
t_ARITHMETIC_OP = r'(\+|-|\*|/)'
t_RELATIONAL_OP = r'(<|>|=|!)'
t_ignore = r" [ \t \n]"
t_Datatype=r'[int float integer INT]'

def t_FLOAT(t):
    r"\d+\.\d+"
    t.value = float(t.value)
    return t


def t_NUMBER(t):
    r'\d+\.\d+|\d+'
    t.value = (t.value)
    return t


def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')
    return t


def t_string(t):
    r'\".*\"'
    t.value = (t.value)
    return t


# def t_COMMENT(t):
#     r'\{(.|\n)*?\}'
#     pass


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


lexer = lex.lex()
# with open ('test.txt','r') as f:
#     contents = f.read()

#######################################################################

input_code = '''
begin
declare int num;
num = 5;
while (x+5 > 10)
do
assign y =0;
endwhile
function add( int x, int y) {
assign x =5;
}
if (x>5) THEN
read z;
else 
write y;
endif
end
'''
########################################################################

import graphviz

def create_node(label, children):
    return (label, children)

def build_graph(node, graph):
    label, children = node
    graph.node(str(id(node)), label)
    for child in children:
        child_node = build_graph(child, graph)
        graph.edge(str(id(node)), str(id(child_node)))
    return node

def visualize_tree(root):
    graph = graphviz.Digraph(format='png')
    build_graph(root, graph)
    graph.render('parse_tree', view=True)

tree =  create_node("Program begin",[
    create_node("Begin",[]),create_node("statements",[]),create_node("End",[])])
tree1 = create_node("Program begin",[
    create_node("Begin",[]),create_node("statements",[create_node("Declaration_STMT",[create_node("declare",[]),create_node("Datatype",[]),create_node("ID",[]),create_node(";",[])]),create_node("statements",[])]),create_node("End",[])])
tree2 = create_node("Program begin",[
    create_node("Begin",[]),create_node("statements",[create_node("Assignment_STMT",[create_node("assign",[]),create_node("ID",[]),create_node("=",[]),create_node("NUMBER",[]),create_node(";",[])]),create_node("statements",[])]),create_node("End",[])])
tree3 = create_node("Program begin",[
    create_node("Begin",[]),create_node("statements",[create_node("Read_STMT",[create_node("read",[]),create_node("ID",[]),create_node(";",[])]),create_node("statements",[])]),create_node("End",[])])
tree4 = create_node("Program begin",[
    create_node("Begin",[]),create_node("statements",[create_node("Write_STMT",[create_node("write",[]),create_node("ID",[]),create_node(";",[])]),create_node("statements",[])]),create_node("End",[])])
tree5 = create_node("Program begin",[
    create_node("Begin",[]),create_node("statements",[create_node("IF_STMT",[create_node("IF",[]),create_node("condition",[create_node("0",[]),create_node("1",[])]),create_node("THEN",[]),create_node("statements",[create_node("IF_STMT",[]),create_node("stataments",[])
    ]),create_node("else",[]),create_node("statements",[create_node("IF_STMT",[]),create_node("stataments",[])])])]),create_node("End",[])])
tree6 = create_node("Program begin",[
    create_node("Begin",[]),create_node("statements",[create_node("WHILE_LOOP",[create_node("while",[]),create_node("condition",[create_node("0",[]),create_node("1",[])]),create_node("do",[]),create_node("statements",[])])]),create_node("End",[])])
tree7= create_node("Program begin",[
    create_node("Begin",[]),create_node("statements",[create_node("FUNCTION",[create_node("function",[]),create_node("ID",[]),create_node("(",[]),create_node("parameters",[create_node("DataType",[]),create_node("ID",[]),create_node(",",[]),create_node("parameters",[])
    ]),create_node(")",[]),create_node("{",[]),create_node("statements",[]),create_node("}",[])]),create_node("statements",[])]),create_node("End",[])])
#visualize_tree(tree1)
# def visual(z):
#     if (z==1):
#         visualize_tree(tree1)
#     elif (z==2):
#         visualize_tree(tree2)
#     elif (z==3):
#         visualize_tree(tree3)
#     elif (z==4):
#         visualize_tree(tree4)
#     elif (z==5):
#         visualize_tree(tree5)
#     elif (z==6):
#         visualize_tree(tree6)
#     elif (z==7):
#         visualize_tree(tree7)
    # else:
    #     visualize_tree(tree)
########################################################################
def parse_program(lexer):
    x=match("begin", lexer)
    print("Program Begin")
    parse_statements_block(lexer)
    

def peek_token(lexer):
    token = get_next_token(lexer)
    return token

def parse_statements_block(lexer):
    while (lexer):
        parse_statement(lexer)
    

def parse_statement(lexer):
    x = peek_token(lexer)
    if x == "declare":
        parse_declare_statement(lexer)
    elif x == "assign":
        parse_assign_statement(lexer)
    elif x =="ID":
        parse_stmt(lexer)
    elif x =="else":
        parse_else(lexer)
    elif x == "if":
        parse_condition(lexer)
    elif x == "while":
        parse_while_loop(lexer)
    elif x == "function":
        parse_function(lexer)
    elif x == "read":
        parse_read_statement(lexer)
    elif x == "write":
        parse_write_statement(lexer)
    elif x== "end" :
        print("Program End")
        visualize_tree(tree7)
        exit(1)
    else:
        return
#########################################################
def parse_declare_statement(lexer):
    
    #match ("declare", lexer)
    datatype = match("Datatype", lexer)
    identifier = match("ID", lexer)
    match("SEMICOLON", lexer)
    print(f"Declare statement: {datatype} {identifier} semicolon")
    


def parse_assign_statement(lexer):
    identifier = match("ID", lexer)
    match("RELATIONAL_OP", lexer)
    num = match("NUMBER",lexer)
    #expression = parse_expression(lexer)
    match("SEMICOLON", lexer)
    print(f"Assign statement: {identifier} = {num} semicolon")
    

def parse_stmt(lexer):
    identifier = "ID"
    match("RELATIONAL_OP", lexer)
    num = match("NUMBER",lexer)
    #expression = parse_expression(lexer)
    match("SEMICOLON", lexer)
    print(f"Expression: {identifier} = {num} semicolon")

def parse_read_statement(lexer):
    #match("read", lexer)
    
    identifier = match("ID", lexer)
    match("SEMICOLON", lexer)
    print(f"Read statement: read {identifier} semicolon")
    


def parse_write_statement(lexer):
    #match("write", lexer)
    #expression = parse_expression(lexer)
    
    identifier = match("ID", lexer)
    match("SEMICOLON", lexer)
    print(f"Write statement: write {identifier} semicolon")
    


def parse_condition(lexer):
    #match("if", lexer)
    
    print("Condition statement : IF condition THEN exp else exp")
    print("condition: ", end = " ")
    condition = parse_expression(lexer)
    match("THEN", lexer)
    print("\nexp: ", end = " ")
    experssion= parse_statement(lexer)
    match("else", lexer)
    print("exp: ", end = " ")
    expression=parse_statement(lexer)
    match("endif", lexer)
    print("ENDIF")
    

def parse_else(lexer):
    match("assign", lexer)
    parse_assign_statement(lexer)
    print("Else Statement")


def parse_while_loop(lexer):
    
    #match("while", lexer)
    print("While statement: While condition DO exp")
    print("condition: ", end = " ")
    condition = parse_expression(lexer)
    # print(f"Condition: ")
    match("do", lexer)
    print("\nexp: ", end =" ")
    parse_statement(lexer)
    match("endwhile", lexer)
    

###########################################################################################
def parse_expression(lexer):
    return parse_arithmetic_expression(lexer)


def parse_arithmetic_expression(lexer):
        parse_term(lexer)

def parse_term(lexer):
    x=peek_token(lexer)
    if x == 'LPAREN':
        #match("LPAREN", lexer)
        expression = parse_expression(lexer)
        #match("RPAREN", lexer)
        return expression
    elif x == 'ID':
        identifier = 'ID'
        print(x, end =" ")
        expression = parse_expression(lexer)
        return identifier
    elif x == 'NUMBER':
        number = 'NUMBER'
        print(x, end =" ")
        expression = parse_expression(lexer)
        return number
    elif x == 'RELATIONAL_OP':
        print(x, end =" ")
        expression =parse_arithmetic_expression(lexer)
    elif x == 'ARITHMETIC_OP':
        print(x, end =" ")
        expression =parse_arithmetic_expression(lexer)
    elif x == 'RPAREN':
        return 
    else:
        raise SyntaxError("Invalid factor")
###################################################################################################################################

def parse_function(lexer):
    
    identifier = match("ID", lexer)
    match("LPAREN", lexer)
    s=parse_parameters(lexer)
    match("RPAREN", lexer)
    print(f"Function: {identifier} {s}")
    print ("{")
    match("LCURL",lexer)
    parse_statement(lexer)
    match("RCURL",lexer)
    print ("}")
    
    
def parse_parameters(lexer):
    datatype = match("Datatype", lexer)
    identifier1 = match("ID", lexer)
    match("COMMA", lexer)
    datatype = match("Datatype", lexer)
    identifier2 = match("ID", lexer)
    return (datatype ,identifier1,datatype ,identifier2)

#########################################################################################

def match(expected_token, lexer):
    token = get_next_token(lexer)
    if token == expected_token:
        return token
    else:
        raise SyntaxError(
            f"Expected token {expected_token}, but found {token}")


def get_next_token(lexer):
    token = lexer.token()
    if token is None:
        return ""
    else:
        return token.type


lexer.input(input_code)
#lexer.input(contents)
parse_program(lexer)
################################################################################################################




