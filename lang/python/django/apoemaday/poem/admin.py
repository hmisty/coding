from django.contrib import admin

from .models import Author, Poem

# Register your models here.

class AuthorAdmin(admin.ModelAdmin):
    list_display = ('name', 'pinyin')
    search_fields = ('name', 'era',)
    list_filter = ('era',)

class PoemAdmin(admin.ModelAdmin):
    list_display = ('title', 'author',)
    search_fields = ('title', 'author__name',)
    list_filter = ('author__name', 'author__era',)

admin.site.register(Author, AuthorAdmin)
admin.site.register(Poem, PoemAdmin)

