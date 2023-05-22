import ply.yacc as yacc
import ply.lex as lex
import os
import codecs
import re

#lexer
resultado_lexer = []

palabrasReservadas = (
    "ver", #print
    "obt", #input
    "es", #if
    "deloc", #else
    "yy", #and
    "oo", #or
    "negar", #not
    "repetir", #whie, for
    "func", #def
    "fin", #break
    "devol" #return
)

literales = (
    "CIERTO",
    "FALSO"
)

tiposDeToken = (
    "IDENTIFICADOR",
    "PALABRA_RESERVADA",
    "LITERAL"
)

tokens = literales + tiposDeToken + (
    "IMPRIMIR",
    "LEER",
    "SI",
    "SINO",
    "Y",
    "O",
    "NEGAR",
    "MIESTRAS",
    "FUNCION",
    "BREAK",
    "RETORNAR",

    "ENTERO",
    "CADENA",
    "NUMERAL",

    "SUMA",
    "RESTA",
    "MULTIPLICACION",
    "DIVISION",
    "MODULO",
    "POTENCIA",

    "ASIGNACION",
    "IGUAL",
    "DIFERENTE",
    "MENOR_IGUAL",
    "MAYOR_IGUAL",
    "MENOR_QUE",
    "MAYOR_QUE",
    "PUNTO_COMA",
    "COMA",
    "PARENTESIS_IZQUIERDO",
    "PARENTESIS_DERECHO",
    "CORCHETE_IZQUIERDO",
    "CORCHETE_DERECHO",
    "LLAVE_IZQUIERDA",
    "LLAVE_DERECHA",
    "BACKSLASH",
    "COMILLA_DOBLE",
    "COMILLA_SIMPLE"
)

t_SUMA = r'\+'
t_RESTA = r'-'
t_MULTIPLICACION = r'\*'
t_DIVISION = r'/'
t_MODULO = r'\%'
t_POTENCIA = r'(\*{2} | \^)'

t_ASIGNACION = r'\:\:'
t_NUMERAL = r'\#'

t_MENOR_QUE = r'\<\<'
t_MAYOR_QUE = r'\>\>'
t_PUNTO_COMA = ';'
t_COMA = r','
t_PARENTESIS_IZQUIERDO = r'\('
t_PARENTESIS_DERECHO = r'\)'
t_CORCHETE_IZQUIERDO = r'\['
t_CORCHETE_DERECHO = r'\]'
t_LLAVE_IZQUIERDA = r'{'
t_LLAVE_DERECHA = r'}'
t_BACKSLASH = r'\\'
t_COMILLA_SIMPLE = r'\''
t_COMILLA_DOBLE = r'\"'

def t_IMPRIMIR(t):
    r'ver'
    return t

def t_LEER(t):
    r'obt'
    return t

def t_SI(t):
    r'es'
    return t

def t_SINO(t):
    r'deloc'
    return t

def t_Y(t):
    r'yy'
    return t

def t_O(t):
    r'oo'
    return t

def t_NEGAR(t):
    r'neg'
    return t

def t_MIESTRAS(t):
    r'repetir'
    return t

def t_FUNCION(t):
    r'func'
    return t

def t_BREAK(t):
    r'fin'
    return t

def t_RETORNAR(t):
    r'devol'
    return t

def t_ENTERO(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_IDENTIFICADOR(t):
    r'\w+(_\d\w)*'
    if t.value in literales:
        t.type = 'LITERAL'
    elif t.value.lower() in palabrasReservadas:
        invalido(t,'Es una palabra reservada')
        return

    return t

def t_CADENA(t):
   r'\"?(\w+ \ *\w*\d* \ *)\"?'
   return t

def t_MENOR_IGUAL(t):
    r'<::'
    return t

def t_MAYOR_IGUAL(t):
    r'>::'
    return t

def t_IGUAL(t):
    r':::'
    return t

def t_DIFERENTE(t):
    r':-:'
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_comments(t):
    r'\*\*\*(.|\n)*?\*\*\*'
    t.lexer.lineno += t.value.count('\n')
    #print("Linea %d inicia comentario de multiples lineas"%(t.lineno))

def t_comments_ONELine(t):
    r'\*\*(.)*\n'
    t.lexer.lineno += 1
    #print("Linea %d comentario"%(t.lineno))

t_ignore =' \t'

def t_error(t):
    global resultado_lexer
    mensaje = "Linea %d -> Token %r invalido." % (t.lineno, str(t.value)[0])
    print(mensaje, "\n")
    resultado_lexer.append(mensaje)
    t.lexer.skip(1)

def invalido(t, arg='Error Indefinido'):
    global resultado_lexer
    mensaje = "Linea %d -> Token %r invalido." % (t.lineno, t.value)
    if arg : mensaje.append(". Descripcion del error: "+arg)
    print(mensaje,"\n")
    resultado_lexer.append(mensaje)

#parser
resultado_parser = []
parser_log = []
variables = {}

precedence = (
    ('right', 'ASIGNACION'),
    ('left', 'IGUAL', 'DIFERENTE'),
    ('left', 'MAYOR_QUE', 'MAYOR_IGUAL', 'MENOR_QUE', 'MENOR_IGUAL'),
    ('left', 'SUMA', 'RESTA'),
    ('left', 'MULTIPLICACION', 'DIVISION', 'MODULO'),
    ('left', 'NEGAR'),
    ('left', 'POTENCIA'),
    ('right', 'NEGATIVO'),
)

def p_declaracionAsignacion(t):
    'declaracion : IDENTIFICADOR ASIGNACION expresion PUNTO_COMA'
    variables[t[1]] = t[3]

def p_declaracionExpresion(t):
    'declaracion : expresion'
    t[0] = t[1]

def p_expresionOperacion(t):
    '''
        expresion :  expresion SUMA expresion
            | expresion RESTA expresion
            | expresion MULTIPLICACION expresion
            | expresion DIVISION expresion
            | expresion POTENCIA expresion
            | expresion MODULO expresion
    '''

    if t[2] == '+':
        t[0] = t[1] + t[3]
    elif t[2] == '-':
        t[0] = t[1] - t[3]
    elif t[2] == '*':
        t[0] = t[1] * t[3]
    elif t[2] == '/':
        t[0] = t[1] / t[3]
    elif t[2] == '%':
        t[0] = t[1] % t[3]
    elif t[2] == '**':
        i = t[3]
        t[0] = t[1]
        while i > 1:
            t[0] *= t[1]
            i -= 1

def p_expresionEnteroNegativo(t):
    'expresion : RESTA expresion %prec NEGATIVO'
    t[0] = -t[2]

def p_grupoExpresiones(t):
    '''
    expresion  : PARENTESIS_IZQUIERDO expresion PARENTESIS_DERECHO
                | LLAVE_IZQUIERDA expresion LLAVE_DERECHA
                | CORCHETE_IZQUIERDO expresion CORCHETE_DERECHO
    '''
    t[0] = t[2]

def p_expresionLogica(t):
    '''
    expresion : expresion MENOR_QUE expresion
        | expresion MAYOR_QUE expresion
        | expresion MENOR_IGUAL expresion
        | expresion MAYOR_IGUAL expresion
        | expresion IGUAL expresion
        | expresion DIFERENTE expresion
        | PARENTESIS_IZQUIERDO expresion PARENTESIS_DERECHO MENOR_QUE PARENTESIS_IZQUIERDO expresion PARENTESIS_DERECHO
        | PARENTESIS_IZQUIERDO expresion PARENTESIS_DERECHO MAYOR_QUE PARENTESIS_IZQUIERDO expresion PARENTESIS_DERECHO
        | PARENTESIS_IZQUIERDO expresion PARENTESIS_DERECHO MENOR_IGUAL PARENTESIS_IZQUIERDO expresion PARENTESIS_DERECHO
        | PARENTESIS_IZQUIERDO expresion PARENTESIS_DERECHO MAYOR_IGUAL PARENTESIS_IZQUIERDO expresion PARENTESIS_DERECHO
        | PARENTESIS_IZQUIERDO expresion PARENTESIS_DERECHO IGUAL PARENTESIS_IZQUIERDO expresion PARENTESIS_DERECHO
        | PARENTESIS_IZQUIERDO expresion PARENTESIS_DERECHO DIFERENTE PARENTESIS_IZQUIERDO expresion PARENTESIS_DERECHO
    '''
    if t[2] == "<<": t[0] = t[1] < t[3]
    elif t[2] == ">>": t[0] = t[1] > t[3]
    elif t[2] == "<::": t[0] = t[1] <= t[3]
    elif t[2] == ">::": t[0] = t[1] >= t[3]
    elif t[2] == ":::": t[0] = t[1] is t[3]
    elif t[2] == ":-:": t[0] = t[1] != t[3]
    elif t[3] == "<<":
        t[0] = t[2] < t[4]
    elif t[2] == ">>":
        t[0] = t[2] > t[4]
    elif t[3] == "<::":
        t[0] = t[2] <= t[4]
    elif t[3] == ">::":
        t[0] = t[2] >= t[4]
    elif t[3] == ":::":
        t[0] = t[2] is t[4]
    elif t[3] == ":-:":
        t[0] = t[2] != t[4]

def p_expresionBooleana(t):
    '''
    expresion   : expresion Y expresion
                | expresion O expresion
                | expresion NEGAR expresion
                | PARENTESIS_IZQUIERDO expresion Y expresion PARENTESIS_DERECHO
                | PARENTESIS_IZQUIERDO expresion O expresion PARENTESIS_DERECHO
                | PARENTESIS_IZQUIERDO expresion NEGAR expresion PARENTESIS_DERECHO
    '''
    if t[2] == "yy":
        t[0] = t[1] and t[3]
    elif t[2] == "oo":
        t[0] = t[1] or t[3]
    elif t[2] == "neg":
        t[0] =  t[1] is not t[3]
    elif t[3] == "yy":
        t[0] = t[2] and t[4]
    elif t[3] == "oo":
        t[0] = t[2] or t[4]
    elif t[3] == "neg":
        t[0] =  t[2] is not t[4]

def p_expresionEntero(t):
    'expresion : ENTERO'
    t[0] = t[1]

def p_expresionCadena(t):
    'expresion : COMILLA_DOBLE expresion COMILLA_DOBLE'
    t[0] = t[2]

def p_expresionIdentificador(t):
    'expresion : IDENTIFICADOR'
    try:
        t[0] = variables[t[1]]
    except LookupError:
        # global resultado_parser
        mensaje = "En la linea {} -> \"{}\" no definido.".format(t.lexer.lineno,t[1])
        # resultado_parser.append(mensaje)
        print(mensaje)
        t[0] = 0

def p_error(t):
    global resultado_parser
    if t:
        mensaje = "En la linea {} -> Error sintactico de tipo {} en el valor {}".format(str(t.lexer.lineno), str(t.type), str(t.value))
        print(mensaje)
    else:
        mensaje = "En la linea {} -> Error sintactico {}".format(str(t.lexer.lineno), t)
        print(mensaje)
    resultado_parser.append(mensaje)

parser = yacc.yacc()

def ejecucion_linea_por_linea(codigo):
    global resultado_gramatica
    resultado_parser.clear()

    for lineaCodigo in codigo.splitlines():
        if lineaCodigo:
            resultado = parser.parse(lineaCodigo)
            if resultado:
                resultado_parser.append(str(resultado))
        else: print("linea de codigo vacia")

    # print("result: ", resultado_parser)
    print('\n'.join(resultado_parser))
    return resultado_parser

'''nombreArchivo =  'code.slx'
ruta = str(os.getcwd())+"/archivos/"+nombreArchivo
fp = codecs.open(ruta,"r","utf-8")
codigoArchivo = fp.read()
fp.close()

analizadorLexico = lex.lex()'''

data=input(5+7)
lexer = lex.lex()
lexer.input(data)
token=lexer.token()
print(format(str(token.value)))