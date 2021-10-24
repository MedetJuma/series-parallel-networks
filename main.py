from ete3 import Tree, TreeStyle

from PIL import Image, ImageFont, ImageDraw

import pathlib

import os

def load_data(path_to_file, cur):
    path_to_file = pathlib.Path(path_to_file)

    data_list = path_to_file.read_text(encoding="utf-8")
    data_list = data_list.split('\n')
    ans = []
    cond = 0
    for element in data_list:
        if element == 'FOR ' + str(cur + 1):
            break
        if element == 'FOR ' + str(cur):
            cond = 1
            continue
        if cond == 1 and len(element) > 0:
            ans.append(element)
    return ans

def add_margin(pil_img, top, right, bottom, left, color):
    width, height = pil_img.size
    new_width = width + right + left
    new_height = height + top + bottom
    result = Image.new(pil_img.mode, (new_width, new_height), color)
    result.paste(pil_img, (left, top))
    return result

ts = TreeStyle()
ts.show_scale = False

path = "/Users/user/Desktop/admissible trees data/sequence for trees.txt"

for cur in range(2, 6):
    file = load_data(path, cur)
    print(file)
    cnt = 0
    dirName = '/Users/user/PycharmProjects/paint_trees/tree_size_' + str(cur)
    # Create target directory & all intermediate directories if don't exists
    try:
        os.makedirs(dirName)
    except FileExistsError:
        pass

    os.chdir(dirName)

    dir1 = dirName
    formula = ''
    for graph in file:
        if graph[0] == 'x' or graph[0] == 'y':
            cnt = 0

            formula = graph
            dir1 = dirName + '/' + graph
            try:
                os.makedirs(dir1)
            except FileExistsError:
                pass
            os.chdir(dir1)
            continue
        ll1 = graph
        num = ''
        graph = ''
        cond = 1
        for char in ll1:
            if char.isdigit() and cond == 1:
                num += char
            elif cond == 0:
                graph += char
            else:
                cond = 0
        graph += ';'
        print(num + " and " + graph)
        t = Tree(graph)
        cnt += 1
        name = 'tree_of_size_' + str(cur) + "_number_" + str(cnt) + '.png'
        print(graph, name)
        t.render(name, w=183, units="mm", tree_style=ts)

        image1 = Image.open(name)
        width, height = image1.size
        image1_new = add_margin(image1, height, 0, height, 0, (255, 255, 255))
        image1_new.save(name)

        image1 = Image.open(name)
        title_font = ImageFont.truetype('/Users/user/Downloads/Roboto/Roboto-Regular.ttf', 30)
        title_text = "Coefficient: " + num
        image_editable = ImageDraw.Draw(image1)
        width, height = image1.size
        image_editable.text((20, height - 40), title_text, (0, 0, 0), font=title_font)
        image1.save(name)