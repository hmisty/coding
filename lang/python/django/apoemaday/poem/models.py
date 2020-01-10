#coding:utf-8
from __future__ import unicode_literals

from django.utils.encoding import python_2_unicode_compatible
from django.utils.translation import gettext_lazy as _
from django.db import models

import pinyin

# Create your models here.

def enum(**enums):
    return type(b'Enum', (), enums)

ERA = enum(UNKNOWN='UNKNOWN', TANG='TANG', SONG='SONG')
ERA_CHOICES = (
    (ERA.UNKNOWN, _('Unknown Era')),
    (ERA.TANG, _('Dynasty Tang')),
    (ERA.SONG, _('Dynasty Song')),
)

# author of a poem
@python_2_unicode_compatible
class Author(models.Model):
    name = models.CharField(max_length=10)
    era = models.CharField(max_length=10, choices=ERA_CHOICES, default=ERA.UNKNOWN)

    # hence we can display the poet name in django admin view
    def __str__(self):
        return self.name

    # non-field property pinyin
    def to_pinyin(self):
        return pinyin.get(self.name)

    to_pinyin.short_description = _('Pinyin name of the author')

    pinyin = property(to_pinyin)


# peom
@python_2_unicode_compatible
class Poem(models.Model):
    title = models.CharField(max_length=255)
    author = models.ForeignKey(Author, related_name='poems')
    content = models.TextField()

    def __str__(self):
        return '%s - %s' % (self.title, self.author.name)

