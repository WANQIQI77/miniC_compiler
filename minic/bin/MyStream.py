#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
@Writer : WanQiQi
@File   : MyStream.py
@Desc   : 重定向控制台输出到控件
"""
from PyQt5.QtCore import QObject, pyqtSignal


class MyStream(QObject):
    """重定向控制台输出到控件

    Attributes:
        new_text: 带一个str类型参数的信号，用于传递字符串
    """

    new_text = pyqtSignal(str)

    def __init__(self, slot):
        super().__init__()
        self.new_text.connect(slot)

    def write(self, text):
        """

        当控制台有输出时发送信号

        :param text: 字符串
        :return:
        """
        self.new_text.emit(str(text))

