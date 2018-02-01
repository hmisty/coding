from PIL import ImageGrab

def grab_image(addr):
    im = ImageGrab.grab()
    im.save(addr, "png")

if __name__ == "__main__":
    grab_image("test.png")
