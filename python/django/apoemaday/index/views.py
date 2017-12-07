# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.shortcuts import render
from django.http import HttpResponse

from .forms import AddForm

# index
def index(request):
    if request.method == 'POST':
        form = AddForm(request.POST)

        if form.is_valid():
            a = form.cleaned_data['a']
            b = form.cleaned_data['b']
            return HttpResponse(str(int(a) + int(b)))
        else:
            pass
    else:
        form = AddForm()
    
    return render(request, 'index.html', {'form': form})

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
