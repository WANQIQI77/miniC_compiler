#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
@Writer : WanQiQi
@File   : Main.py
@Desc   : 程序主函数
"""
import sys

from PyQt5.QtWidgets import QApplication

from MyMainWindow import MyMainWindow

if __name__ == '__main__':
    app = QApplication(sys.argv)
    mw = MyMainWindow()
    mw.show()
    sys.exit(app.exec_())
