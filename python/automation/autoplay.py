#!/usr/bin/env python
#coding: utf-8
# ref: http://blog.csdn.net/jin123wang/article/details/78988220

import time  
import datetime  
import sys  
import math  
from PIL import Image, ImageTk  
from PIL import ImageGrab  
from pymouse import PyMouse  
if sys.version_info.major == 2:  
    import Tkinter as tk  
else:  
    import tkinter as tk  
  
BBOX_LEFT = 30  
BBOX_RIGHT = 360 + 600
BBOX_TOP = 200 + 300
BBOX_BOTTOM = 480 + 900
IMG_RATIO = 1.0 #2.5
BUTTON_ID = 1  
GRID_SIZE = 20  
MARK_SIZE = 20  
POS_SIZE = 10  
  
TIME_COE = 3.90/IMG_RATIO  
  
  
class GUI(tk.Tk, object):  
    def __init__(self):  
        super(GUI, self).__init__()  
        # self.grab_set_global()  
        self.start_pos = [0, 0]  
        self.end_pos = [0, 0]  
        self.st = 0  
        self.ms = PyMouse()  
        self.c_mark_init = 0  
        self._build_gui()  
        self.bind('<KeyPress-j>', self._CaptureScreen) #press 'j' key to capture
        self.bind('<Motion>', self._MouseMotion)  
        self.bind('<Button-1>', self._MarkPosition) #left click mouse to mark

    def _build_gui(self):  
        self.title('Jumper Broker')  
        w = int((BBOX_RIGHT - BBOX_LEFT)*IMG_RATIO)  
        h = int((BBOX_BOTTOM - BBOX_TOP)*IMG_RATIO)  
        self.geometry('{0}x{1}'.format(w,h))  
        self.canvas = tk.Canvas(self, width = w, height = h)  
        self.canvas.pack()  
  
      
    def _grab_image(self):  
        #grab and save the image  
        img = ImageGrab.grab(bbox=(BBOX_LEFT,BBOX_TOP, BBOX_RIGHT, BBOX_BOTTOM))  
        (w,h) = img.size  
        w = int(w * IMG_RATIO)  
        h = int(h * IMG_RATIO)  
        img2 = img.resize((w, h), Image.ANTIALIAS)  
        global g_tk_image  
        g_tk_image = ImageTk.PhotoImage(img2)  
        self.canvas.delete( 'all')  
        self.canvas.create_image(w/2, h/2, image = g_tk_image)  
        ##create some grids  
        w_num = int(w/GRID_SIZE)  
        h_num = int(h/GRID_SIZE)  
        for i in range(w_num):  
            self.canvas.create_line(i*GRID_SIZE, 0, i*GRID_SIZE, h)  
        for j in range(h_num):  
            self.canvas.create_line(0, j*GRID_SIZE, w, j*GRID_SIZE)  
  
  
    def _Click(self):  
        dx = self.end_pos[0] - self.start_pos[0]  
        dy = self.end_pos[1] - self.start_pos[1]  
        len = math.sqrt(dx * dx + dy * dy)  
        t = len * TIME_COE  
        print(dx, dy, t)  
  
        self.ms.press(BBOX_LEFT, BBOX_TOP, BUTTON_ID)  
        self.after(int(t), self._over)  
      
    def _over(self):  
        self.ms.release(BBOX_LEFT, BBOX_TOP,BUTTON_ID)  
        self.focus_force()  
  
    def _DrawPos(self):  
        #draw the start position  
        if self.st == 1:  
            x = self.start_pos[0]  
            y = self.start_pos[1]  
        elif self.st == 2:  
            x = self.end_pos[0]  
            y = self.end_pos[1]  
        self.canvas.create_oval(x-POS_SIZE, y-POS_SIZE, x+POS_SIZE, y+POS_SIZE, fill = 'yellow')  
  
  
    def _CaptureScreen(self, event):  
        #step 0, grab the picture  
        if(self.st == 0):  
            print('event to grab image')
            self._grab_image()  
            self.st = 1  

    def _MarkPosition(self, event):
        print('clicked at (%s, %s)' % (event.x, event.y))
  
        #step 1, set set the first position  
        if(self.st == 1):  
            print('event to set begin position (%s, %s)' % (event.x, event.y))
            self.start_pos = [event.x, event.y]  
            self._DrawPos()  
            self.st = 2  
        elif(self.st == 2):  
            print('event to set end position (%s, %s)' % (event.x, event.y))
            self.end_pos = [event.x, event.y]  
            self._DrawPos()  
            self._Click()  
            self.st = 0  
      
    def _MouseMotion(self, event):  
        x, y = event.x, event.y  
        if self.c_mark_init == 1:  
            self.canvas.delete(self.c_mark_h)  
            self.canvas.delete(self.c_mark_v)  
        self.c_mark_h = self.canvas.create_line(x - MARK_SIZE, y, x + MARK_SIZE, y, fill = 'red', width = 2)  
        self.c_mark_v = self.canvas.create_line(x, y - MARK_SIZE, x, y + MARK_SIZE, fill = 'red', width = 2)  
        self.c_mark_init = 1  
  
if __name__ == "__main__":  
    g_tk_image = None  
    gui = GUI()  
    gui.mainloop()  
