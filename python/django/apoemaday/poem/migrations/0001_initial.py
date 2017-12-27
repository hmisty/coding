# -*- coding: utf-8 -*-
# Generated by Django 1.11.5 on 2017-12-07 07:46
from __future__ import unicode_literals

from django.db import migrations, models
import django.db.models.deletion


class Migration(migrations.Migration):

    initial = True

    dependencies = [
    ]

    operations = [
        migrations.CreateModel(
            name='Author',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('name', models.CharField(max_length=10)),
                ('era', models.CharField(choices=[('UNKNOWN', '\u672a\u77e5'), ('TANG', '\u5510'), ('SONG', '\u5b8b')], default='UNKNOWN', max_length=10)),
            ],
        ),
        migrations.CreateModel(
            name='Poem',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('title', models.CharField(max_length=255)),
                ('content', models.TextField()),
                ('author', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='poems', to='poem.Author')),
            ],
        ),
    ]