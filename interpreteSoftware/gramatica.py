import sys 
sys.path.insert(0,"../..")

if sys.version_info[0] >= 3:
    raw_input = input

import ply.lex as lex
import ply.yacc as yacc
import os 

class Parser(object):
    
    tokens = ()
    precedence = ()

    def __init__(self, **kw): 
        self.debug = kw.get('debug',0)
        self.names = { } 

        try:
            modname = os.path.split(os.path.splitext(__file__)[0])[1] + "_" + self.__class__.__name__
        except:
            modname = "parser"+"_"+self.__class__.__name__
        self.debugfile = modname + ".dbg"
        self.tabmodule = modname + "_" + "parsetab"
        #print self.debugfile, self.tabmodule

        #Build the lexer and parser
        lex.lex(module=self, debug=self.debug)
        yacc.yacc(module=self,
                    debug=self.debug,
                    debugfile=self.debugfile,
                    tabmodule=self.tabmodule)

    def run(self):
        while 1:
            try:
                s = raw_input('calc > ')
            except EOFError:
                break
            if not s: continue
            yacc.parse(s)

class Calc(Parser):
    tokens = [
        'NAME',
        'NUMBER',
        'PLUS',
        'MINUS',
        'EXP',
        'TIMES',
        'DIVIDE',
        'EQUALS',
        'LPAREN',
        'RPAREN',
        'MENOR',
        'MAYOR',
        'MENORIGUAL',
        'MAYORIGUAL',
        'IGUAL',
        'DISTINTO'
    ]

    #tokens
    t_plus      = r'\+'
    t_MINUS     = r'-'
    t_EXP       = r'\*\*'
    t_TIMES     = r'\*'
    t_DIVIDE    = r'/'
    t_EQUALS    = r'='
    t_LPAREN    = r'\('
    t_RPAREN    = r'\)'
    t_NAME      = r'[a-zA-Z_][a-aA-Z0-9_]*'
    t_SI        = r'si'

    def t_NUMBER(self, t):
        r'\d+'
        try:
            t.value = int(t.value)
        except ValueError:
            print("Integer value too large %s" % t.value)
            t.value = 0
            
        return t
    t_ignore = " \t"

    def t_newline(self, t):
        r'\n+'
        t.lexer.lineno += t.value.count("\n")

    def t_error(self, t):
        print("Illegal character '%s'" % t.value[0])
        t.lexer.skip(1)

    #parsing rules

    precedence = (
        ('left','PLUS','MINUS'),
        ('left', 'TIMES', 'DIVIDE'),
        ('left', 'EXP'),
        ('right','UMINUS'),
    )
    def p_statement_assign(self, p):
        'statement : NAME EQUALS expression'
        self.names[p[1]]  = p[3]

    def p_statement_expr(self, p):
        'statement : expression'
        print(p[1])

    def p_expression_binop(self, p):
        """
        expression : expression PLUS expression
                    | expression MINUS expression
                    | expression TIMES expression
                    | expression DIVIDE expression
                    | expression EXP expression
        """
        
        if p[2] == '+' : p[0] = p[1] + p[3]
        elif p[2] == '-' : p[0] = p[1] - p[3]
        elif p[2] == '*' : p[0] = p[1] * p[3]
        elif p[2] == '/' : p[0] = p[1] / p[3]
        elif p[2] == '**' : p[0] = p[1] ** p[3]

    def p_expression_condicional(self, p):
        """
        expression : expression MENOR expression
                    | expression MAYOR expression
                    | expression MENORIGUAL expression
                    | expression MAYORIGUAL expression
                    | expression IGUAL expression 
                    | expression DISTINTO expression

        """
        
        if p[2] == '<': p[0] = p[1] < p[3]
        elif p[2] == '>': p[0] = p[1] > p[3]
        elif p[2] == '<=': p[0] = p[1] <= p[3]
        elif p[2] == '>=': p[0] = p[1] >= p[3]
        elif p[2] == '==': p[0] = p[1] == p[3]
        elif p[2] == '!=': p[0] = p[1] != p[3]
        

    def p_expression_uminus(self, p):
        'expression : MINUS expression %prec UMINUS'
        p[0]= -p[2]

    def p_expression_group(self, p):
        'expression : LPAREN expression RPAREN'
        p[0] = p[2]

    def p_expression_number(self, p):
        'expression : NUMBER'
        p[0] = p[1]

    def p_expression_name(self, p):
        'expression : NAME'
        try:
            p[0] = self.names[p[1]]
        except LookupError:
            print("Undefined name '%s'" % p[1])
            p[0] = 0

    def p_expression_if(self, p):
        'expression : SI expression'
        print("Conditional if statement") 
        p[0] = p[1] > p[2] 

    def p_error(self, p):
        if p:
            print("Syntax error at '%s'" % p.value)
        else:
            print("Syntax error at EOF")


if __name__ == '__main__':
    calc = Calc()
    calc.run()
