from django.contrib import admin

from .models import Author, Poem

# Register your models here.

class AuthorAdmin(admin.ModelAdmin):
    list_display = ('name', 'pinyin')

class PoemAdmin(admin.ModelAdmin):
    list_display = ('title', 'author',)

admin.site.register(Author, AuthorAdmin)
admin.site.register(Poem, PoemAdmin)

