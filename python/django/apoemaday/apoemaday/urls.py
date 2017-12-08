"""apoemaday URL Configuration

The `urlpatterns` list routes URLs to views. For more information please see:
    https://docs.djangoproject.com/en/1.11/topics/http/urls/
Examples:
Function views
    1. Add an import:  from my_app import views
    2. Add a URL to urlpatterns:  url(r'^$', views.home, name='home')
Class-based views
    1. Add an import:  from other_app.views import Home
    2. Add a URL to urlpatterns:  url(r'^$', Home.as_view(), name='home')
Including another URLconf
    1. Import the include() function: from django.conf.urls import url, include
    2. Add a URL to urlpatterns:  url(r'^blog/', include('blog.urls'))
"""
from django.conf.urls import url
from django.conf.urls.i18n import i18n_patterns
from django.contrib import admin

from index import views as index_views

urlpatterns = [
    #url(r'^$', index_views.home, name = 'home'),
    url(r'^$', index_views.index, name = 'index'),
    url(r'^add/$', index_views.add, name='add'),
    url(r'^add/(\d+)/(\d+)/$', index_views.add2, name='add2'),
    #url(r'^admin/', admin.site.urls),
]

urlpatterns += i18n_patterns(
    url(r'^admin/', admin.site.urls),
)

