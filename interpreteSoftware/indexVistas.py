from django.http import HttpResponse 
from django.template import Template, Context
from django.views.decorators.csrf import csrf_exempt
from . import gramatica

class CrearInterprete:
    @csrf_exempt
    def interprete(request):
        docExterno=open("C:/Users/user/Desktop/grabaciones y clases unimag/SEMESTRE 9/COMPILADORES/tareas/interpreteSoftware/interpreteSoftware/vista/static/interprete.html")
        plt=Template(docExterno.read())
        docExterno.close()
        ctx=Context()
        documento=plt.render(ctx)
        return HttpResponse(documento)