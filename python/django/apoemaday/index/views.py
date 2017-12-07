# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.shortcuts import render
from django.http import HttpResponse

# index
def index(request):
    return HttpResponse(u'你好张哥！')

# add?a=1&b=2
def add(request):
    a = request.GET['a']
    b = request.GET['b']
    c = int(a) + int(b)
    return HttpResponse(str(c))

# add/1/2
def add2(request, a, b):
    c = int(a) + int(b)
    return HttpResponse(str(c))

# home
def home(request):
    return render(request, 'home.html')
