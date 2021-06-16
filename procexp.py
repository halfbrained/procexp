#!/usr/bin/python3
# This file is part of the Linux Process Explorer
# See www.sourceforge.net/projects/procexp
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA

#Thanks to the following developers helping:
#
#  Diaa Sami, making the GUI more usable
#


#create qt app early, in order to show unhandled exceptions graphically.
import os
import psutil

pid = os.getpid()
def ram(name):

    py = psutil.Process(pid)

    mem = py.memory_info()[0] / 1024 / 1024
    print(f'ram: {mem:5.1f}MB: {name}')


import sys
ram('sys')
from PyQt5 import QtCore, QtGui, QtWidgets, uic
ram('qt')
import qwt
ram('qwt')
import utils.procutils
ram('ut-proc')

import procreader.reader
import logui
import aboutui
import singleprocess
import systemoverview
import configobj
import settings as settingsMenu
ram('. 1')

import plotobjects
import networkoverview
import colorlegend
ram('. 2')

import cpuaffinity
import signal
import procreader.tcpip_stat as tcpip_stat
import rootproxy
import messageui
ram('. 3')

#DBG
from ipdb import set_trace as bp #USE: bp()
ram('. ipdb')


g_timer = None
g_reader = None
g_treeProcesses = {} #flat dictionary of processes; pid -> QTreeWidgetItem
g_toplevelItems = {}
g_mainUi = None
g_onlyUser = True
g_greenTopLevelItems = {}
g_redTopLevelItems = {}
g_singleProcessUiList = {}
g_curveCpuHist = None
g_curveCpuSystemHist = None
g_curveIoWaitHist = None
g_curveIrqHist = None
g_curveCpuPlotGrid = None
g_cpuUsageHistory = None
g_cpuUsageSystemHistory = None
g_cpuUsageIoWaitHistory = None
g_cpuUsageIrqHistory = None
g_systemOverviewUi = None
g_networkOverviewUi = None
g_mainWindow = None
g_firstUpdate = True
g_procList = {}

#default settings
g_settings = {}
g_defaultSettings = {
    "fontSize": 10,
    "columnWidths": [100,60,40,100,30,30,30,30],
    "updateTimer": 1000,
    "historySampleCount": 200,
    "hiddenColumns": [],
}

g_treeViewcolumns = ["Process","PID","CPU","Command Line", "User", "Chan","#thread"]


def _get_top_proc(history_fraction):
    frac = history_fraction
    # get length of history  => fraction to index  => proc with max cpu_usage on that history_index
    if g_procList  and  0 <= frac <= 1:
        rnd_hist = list(g_procList.values())[0]['history']
        ind = round(frac*rnd_hist.HistoryDepth)
        _pid_hists = ((pid, d['history']) for pid,d in g_procList.items())
        _pid_usages = ((pid, hist.cpuUsageHistory[ind] + hist.cpuUsageKernelHistory[ind])
                                                                        for pid,hist in _pid_hists)
        top_pid, usage = max(_pid_usages, key=lambda it: it[1])

        proc = g_procList[top_pid]
        history = proc['history']
        glob_usages = [g_cpuUsageHistory[ind], g_cpuUsageSystemHistory[ind], g_cpuUsageIrqHistory[ind],
                g_cpuUsageIoWaitHistory[ind]]
        glob_usages = ['{:.1f}'.format(val) for val in glob_usages]
        hint_lines = [
            f'{proc["name"]} ({top_pid}): {usage:.1f}% / {sum(glob_usages):.1f}%  {glob_usages}',
            f'cpu: {history.cpuUsageHistory[ind]:.1f}%',
            f'kernel: {history.cpuUsageKernelHistory[ind]:.1f}%',
            f'IO: {history.IOHistory[ind]:.1f}',
        ]
        return '\n'.join(hint_lines)
    return f'bad frac: {frac}'

def _is_tree_item_visible(it):
    parent = it.parent()
    while parent:
        if not parent.isExpanded():
            return False
        parent = parent.parent()

    return True

def _apply_tree_altcolors():
    def scan_apply(parent, nvis=0):
        parent_expanded = parent == root  or  parent.isExpanded() # root is not expanded...
        for i in range(parent.childCount()):
            ch = parent.child(i)
            if parent_expanded:
                nvis += 1
                color = baseBgColor if nvis%2 == 0 else  altBgColor
                for column in range(ch.columnCount()):
                    ch.setBackground(column, color)
                nvis = scan_apply(ch, nvis)
        return nvis
    #end scan_apply()

    root = g_mainUi.processTreeWidget.invisibleRootItem()
    baseBgColor = g_mainUi.processTreeWidget.palette().color(QtGui.QPalette.Base)
    altBgColor = g_mainUi.processTreeWidget.palette().color(QtGui.QPalette.AlternateBase)
    scan_apply(root)



def performMenuAction(action):
    ''' perform action from menu
    '''
    global g_procList
    global g_onlyUser
    global g_settings
    global g_systemOverviewUi
    global g_networkOverviewUi
    if action is g_mainUi.actionKill_process:
        try:
            selectedItem = g_mainUi.processTreeWidget.selectedItems()[0]
        except IndexError:
            return
        process = selectedItem.data(1,0).toString()
        utils.procutils.killProcessHard(process)

    elif action is g_mainUi.actionKill_process_tree:
        try:
            selectedItem = g_mainUi.processTreeWidget.selectedItems()[0]
        except IndexError:
            return
        process = selectedItem.data(1,0).toString()
        killProcessTree(process, g_procList)

    elif action is g_mainUi.actionShow_process_from_all_users:
        if g_onlyUser:
            g_reader.noFilterUID()
            clearTree()
            g_onlyUser = False
        else:
            g_reader.setFilterUID(os.geteuid())
            clearTree()
            g_onlyUser = True

    elif action is g_mainUi.actionProperties:
        try:
            selectedItem = g_mainUi.processTreeWidget.selectedItems()[0]
        except IndexError:
            return
        process = str(selectedItem.data(1,0))
        if process in g_singleProcessUiList.keys():
            g_singleProcessUiList[process].makeVisible()
        else:
            if int(process) in g_procList.keys():
                g_singleProcessUiList[process] = singleprocess.singleUi(
                        process,
                        g_procList[int(process)]["cmdline"],
                        g_procList[int(process)]["name"],
                        g_reader,
                        int(g_settings["historySampleCount"]),
                )
    elif action is g_mainUi.actionSaveSettings:
        saveSettings()
    elif action is g_mainUi.actionSettings:
        msec, depth, fontSize = settingsMenu.doSettings(
                int(g_settings["updateTimer"]),
                int(g_settings["historySampleCount"]),
                int(g_settings["fontSize"]),
        )
        g_settings["updateTimer"] = int(msec)
        g_settings["historySampleCount"] = int(depth)
        g_settings["fontSize"] = int(fontSize)
        setFontSize(fontSize)
    elif action is g_mainUi.actionSystem_information:
        g_systemOverviewUi.show()
    elif action is g_mainUi.actionNetwork_Information:
        g_networkOverviewUi.show()
    elif action is g_mainUi.actionClose_all_and_exit:
        for window in g_singleProcessUiList:
            g_singleProcessUiList[window].closeWindow()
        g_systemOverviewUi.close()
        g_networkOverviewUi.close()
        g_mainWindow.close()
        logui.closeLogWindow()
    elif action is g_mainUi.actionColor_legend:
        colorlegend.doColorHelpLegend()
    elif action is g_mainUi.actionSet_affinity:
        try:
            selectedItem = g_mainUi.processTreeWidget.selectedItems()[0]
            process = str(selectedItem.data(1,0))
        except IndexError:
            return
        cpuaffinity.doAffinity(g_reader.getCpuCount(), process)
    elif action is g_mainUi.actionLog:
        logui.doLogWindow()
    elif action is g_mainUi.actionAbout:
        aboutui.doAboutWindow()
    elif action is g_mainUi.actionClear_Messages:
        messageui.clearAllMessages()
    else:
        utils.procutils.log("This action (%s)is not yet supported." %action)

'!!! MY'

class CanvasPicker(QtCore.QObject):
        def __init__(self, plot):
                QtCore.QObject.__init__(self, plot)
                #self.__selectedCurve = None
                #self.__selectedPoint = -1
                self.__plot = plot
                canvas = plot.canvas()
                canvas.installEventFilter(self)
                # We want the focus, but no focus rect.
                # The selected point will be highlighted instead.
                canvas.setFocusPolicy(QtCore.Qt.StrongFocus)
                canvas.setCursor(QtCore.Qt.PointingHandCursor)
                canvas.setFocusIndicator(qwt.QwtPlotCanvas.ItemFocusIndicator)
                canvas.setFocus()
                '???'
                #self.__shiftCurveCursor(True)

                #plot.setToolTip('!Crap!')
                self._last_mouse_glob = None
                self._last_mouse_loc = None

        def event(self, event):
                if event.type() == QtCore.QEvent.User:
                        self.__showCursor(True)
                        return True
                return QtCore.QObject.event(self, event)

        def eventFilter(self, object, event):
                #if event.type() == QtCore.QEvent.MouseMove:
                        #self.__move(event.pos())
                        #print(f'mouse -- {event, event.pos()}')
                        #bp()
                        #self.__plot.setToolTip(f'mouse -- {event, event.pos()}')
                        #return True
                        #return False
                if event.type() == QtCore.QEvent.ToolTip:
                        self._last_mouse_glob = event.globalPos()
                        self._last_mouse_loc = event.pos()
                        fraction = self._last_mouse_loc.x() / self.__plot.geometry().width()
                        #bp()
                        #self.__plot.setToolTip(f'mouse -- {event, event.pos()}')
                        ttt = _get_top_proc(fraction)
                        self.__plot.setToolTip(ttt)
                        #print(f'--updated ToolTip')

                        #return True
                elif event.type() == QtCore.QEvent.Paint:
                        #bp()
                        if type(self.__plot.toolTip()) is str:
                            #self.__plot.setToolTip(ttt)
                            #self.event(QtCore.QEvent(QtCore.QEvent.ToolTipChange))
                            #self.event(QtCore.QEvent(QtCore.QEvent.ToolTip))
                            #print(f'painting...: {self.__plot.toolTip()}')
                            #self.__plot.setToolTip(ttt)
                            if self._last_mouse_glob:
                                fraction = self._last_mouse_loc.x() / self.__plot.geometry().width()
                                ttt = _get_top_proc(fraction)
                                QtWidgets.QToolTip.showText(self._last_mouse_glob, ttt)
                        #return True
                elif event.type() == QtCore.QEvent.Leave:
                    self._last_mouse_glob = None
                    self._last_mouse_loc = None

                else:
                        #print(f'-------other ev: {event, event.type(), dir(event)}')
                        print(f'-------other ev: {event, event.type()}')
                return False

        def __showCursor(self, showIt):
                curve = self.__selectedCurve
                if not curve:
                        return
                symbol = curve.symbol()
                brush = symbol.brush()
                if showIt:
                        symbol.setBrush(symbol.brush().color().darker(180))
                curve.directPaint(self.__selectedPoint, self.__selectedPoint)
                if showIt:
                        symbol.setBrush(brush)

        def __shiftCurveCursor(self, up):
                curves = [
                        curve for curve in self.__plot.itemList() if isinstance(curve, QwtPlotCurve)
                ]
                if not curves:
                        return
                if self.__selectedCurve in curves:
                        index = curves.index(self.__selectedCurve)
                        if up:
                                index += 1
                        else:
                                index -= 1
                        # keep index within [0, len(curves))
                        index += len(curves)
                        index %= len(curves)
                else:
                        index = 0
                self.__showCursor(False)
                self.__selectedPoint = 0
                self.__selectedCurve = curves[index]
                self.__showCursor(True)

_cpu_hist_picker = None


def setFontSize(fontSize):
    global g_settings
    g_settings["fontSize"] = fontSize
    font = QtGui.QFont()
    font.setPointSize(fontSize)
    g_mainUi.menuFile.setFont(font)
    g_mainUi.menuOptions.setFont(font)
    g_mainUi.menuView.setFont(font)
    g_mainUi.menuProcess.setFont(font)
    g_mainUi.menuSettings.setFont(font)
    g_mainUi.menubar.setFont(font)
    g_mainUi.processTreeWidget.setFont(font)
    if g_systemOverviewUi is not None:
        g_systemOverviewUi.setFontSize(fontSize)
    if g_networkOverviewUi is not None:
        g_networkOverviewUi.setFontSize(fontSize)


def loadSettings():
    global g_settings
    settingsPath = os.path.expanduser("~/.procexp/settings")
    if os.path.exists(settingsPath):
        with open(settingsPath,"rb") as f:
            settingsObj = configobj.ConfigObj(infile=f)
        g_settings=settingsObj.dict()

    #load default settings for undefined settings
    for item in g_defaultSettings:
        if item in g_settings.keys():
            pass
        else:
            g_settings[item] = g_defaultSettings[item]

    fontsize = int(g_settings["fontSize"])
    setFontSize(fontsize)

    #set the columnwidths
    for headerSection in range(g_mainUi.processTreeWidget.header().count()):
        try:
            width = int(g_settings["columnWidths"][headerSection])
        except:
            width = 150
        g_mainUi.processTreeWidget.header().resizeSection(headerSection,width)

    #load default settings for undefined settings
    for item in g_defaultSettings:
        if item in g_settings.keys():
            pass
        else:
            g_settings[item] = g_defaultSettings[item]

    # load hidden columns
    for i,col in enumerate(g_treeViewcolumns):
        if col in g_settings['hiddenColumns']:
            g_mainUi.processTreeWidget.setColumnHidden(i, True)

    global g_cpuUsageHistory
    global g_cpuUsageSystemHistory
    global g_cpuUsageIoWaitHistory
    global g_cpuUsageIrqHistory

    g_cpuUsageHistory = [0] * int(g_settings["historySampleCount"])
    g_cpuUsageSystemHistory = [0] * int(g_settings["historySampleCount"])
    g_cpuUsageIoWaitHistory = [0] * int(g_settings["historySampleCount"])
    g_cpuUsageIrqHistory = [0] * int(g_settings["historySampleCount"])


def saveSettings():
    '''save settings to ~.procexp directory, in file "settings"
    '''
    widths = []
    for headerSection in range(g_mainUi.processTreeWidget.header().count()):
        widths.append(g_mainUi.processTreeWidget.header().sectionSize(headerSection))
    g_settings["columnWidths"] = widths

    for i, col in enumerate(g_treeViewcolumns):
        if g_mainUi.processTreeWidget.isColumnHidden(i):
            g_settings['hiddenColumns'].append(col)

    settingsPath = os.path.expanduser("~/.procexp")
    if not(os.path.exists(settingsPath)):
        os.makedirs(settingsPath)
    cfg = configobj.ConfigObj(g_settings)
    with open(settingsPath + "/settings","wb") as f:
        cfg.write(f)

def onContextMenu(point):
    global g_mainUi
    g_mainUi.menuProcess.exec_(g_mainUi.processTreeWidget.mapToGlobal(point))

def onHeaderContextMenu(point):
    menu = QtWidgets.QMenu()
    for idx, col in enumerate(g_treeViewcolumns):
        action = QtWidgets.QAction(col, g_mainUi.processTreeWidget)
        action.setCheckable(True)
        action.setChecked(not g_mainUi.processTreeWidget.isColumnHidden(idx))
        action.setData(idx)
        menu.addAction(action)
    selectedItem = menu.exec_(g_mainUi.processTreeWidget.mapToGlobal(point))
    if selectedItem is not None:
        #g_mainUi.processTreeWidget.setColumnHidden(selectedItem.data().toInt()[0], not selectedItem.isChecked())
        g_mainUi.processTreeWidget.setColumnHidden(selectedItem.data(), not selectedItem.isChecked())

# flat or tree
def set_tree_type(_type):
    if _type == 'flat':
        print(f' converting to FLAT')
        # remove all children, then add to top level
        children = []  # all removed children
        for i in range(g_mainUi.processTreeWidget.topLevelItemCount()):
            topLevelItem = g_mainUi.processTreeWidget.topLevelItem(i)
            take_all_children(topLevelItem, children)

        for item in children:
            g_mainUi.processTreeWidget.addTopLevelItem(item)

    elif _type == 'tree': # convert back to tree
        print(f' converting to TREE')
        tree = g_mainUi.processTreeWidget
        children_map = {} # pid -> list of child pids
        root = tree.invisibleRootItem()
        for pid in g_procList:
            ppid = g_procList[pid]["PPID"]
            children_map.setdefault(ppid, []).append(pid)
            # remove from tree list
            if ppid > 0  and  ppid in g_procList:
                index = root.indexOfChild(g_treeProcesses[pid])
                tree.takeTopLevelItem(index)

        # add children to parents accoding to `children_map`
        tree_items = [tree.topLevelItem(i)  for i in range(tree.topLevelItemCount())]
        pids_in_tree = [pid for pid,item in g_treeProcesses.items()  if item in tree_items]
        while pids_in_tree:
            pid = pids_in_tree.pop(0) # take first pid in queue
            children_pids = children_map.get(pid, [])
            if children_pids:    # if have children - add children items
                children_items = [g_treeProcesses[ch_pid] for ch_pid in children_pids]
                item = g_treeProcesses[pid]
                item.addChildren(children_items)

                pids_in_tree.extend(children_pids)

        g_mainUi.processTreeWidget.expandAll()
    else:
        raise Exception(f'moron {_type}')


_ignore_sort = False

def _on_sort(icolumn, order):
    # column not 0 - flat list
    # col-0: states 0,1,2

    global _ignore_sort

    if _ignore_sort:
        return

    header = g_mainUi.processTreeWidget.header()
    is_indicator = header.isSortIndicatorShown()

    #0,1 => 1,1 -> 0,0! -> 1,1!

    if icolumn == 0:
        if order == 0  and  is_indicator:     # 1 (1,1) -> 2 (1,0)
            try:
                _ignore_sort = True
                g_mainUi.processTreeWidget.sortItems(0, 1) # column=0, order=1
            finally:
                _ignore_sort = False
            header.setSortIndicatorShown(False)
            print(f' 1 -> 2')

        elif order == 0:
            #if not is_indicator:   # 2 (1,0) -> 0 (0,1)
            header.setSortIndicatorShown(True)
            set_tree_type('tree')
            print(f' 2 -> 0')

        elif order == 1: #  and  is_indicator:     # 0 (0,1) -> 1 (1,1)
            #header.setSortIndicatorShown(True)
            print(f' 0 -> 1')
            set_tree_type('flat')
    else:
        if not is_indicator:
            header.setSortIndicatorShown(True)
        set_tree_type('flat')


def prepareUI(mainUi):
    """ prepare the main UI, setup plots and menu triggers
    """
    global g_timer

    ram('<prepare ui')

    palette = app.palette()
    palette.setColor(palette.AlternateBase, QtGui.QColor(0xd8, 0xd8, 0xd8))
    palette.setColor(palette.Background,    QtGui.QColor(0xe0, 0xe0, 0xe0))
    palette.setColor(palette.Base,          QtGui.QColor(0xe0, 0xe0, 0xe0))
    app.setPalette(palette)
    #pal = g_mainUi.processTreeWidget.palette()
    #g_mainUi.processTreeWidget.setPalette(pal)

    #g_mainUi.processTreeWidget.header().clicked.connect(on_click)
    #g_mainUi.processTreeWidget.header().clicked.connect(on_click)
    g_mainUi.processTreeWidget.header().sortIndicatorChanged.connect(_on_sort)


    mainUi.processTreeWidget.setSortingEnabled(True)
    mainUi.processTreeWidget.setIndentation(10)
    #mainUi.processTreeWidget.alternatingRowColors = True # doesn't work without a 'model'?
    mainUi.processTreeWidget.setColumnCount(len(g_treeViewcolumns))

    mainUi.processTreeWidget.setHeaderLabels(g_treeViewcolumns)
    mainUi.processTreeWidget.header().setContextMenuPolicy(QtCore.Qt.CustomContextMenu)
    mainUi.processTreeWidget.header().customContextMenuRequested.connect(onHeaderContextMenu)

    #create a timer which triggers the process explorer to update its contents
    g_timer = QtCore.QTimer(mainUi.processTreeWidget)
    g_timer.timeout.connect(updateUI)
    # QtCore.QObject.connect(g_timer, QtCore.SIGNAL("timeout()"), updateUI)
    mainUi.processTreeWidget.customContextMenuRequested.connect(onContextMenu)
    # QtCore.QObject.connect(mainUi.processTreeWidget, QtCore.SIGNAL('customContextMenuRequested(const QPoint&)'), onContextMenu)
    mainUi.menuFile.triggered.connect(performMenuAction)
    # QtCore.QObject.connect(mainUi.menuFile,  QtCore.SIGNAL('triggered(QAction*)'), performMenuAction)
    mainUi.menuProcess.triggered.connect(performMenuAction)
    # QtCore.QObject.connect(mainUi.menuProcess,  QtCore.SIGNAL('triggered(QAction*)'), performMenuAction)
    mainUi.menuOptions.triggered.connect(performMenuAction)
    # QtCore.QObject.connect(mainUi.menuOptions,  QtCore.SIGNAL('triggered(QAction*)'), performMenuAction)
    mainUi.menuSettings.triggered.connect(performMenuAction)
    # QtCore.QObject.connect(mainUi.menuSettings, QtCore.SIGNAL('triggered(QAction*)'), performMenuAction)
    mainUi.menuView.triggered.connect(performMenuAction)
    # QtCore.QObject.connect(mainUi.menuView, QtCore.SIGNAL('triggered(QAction*)'), performMenuAction)
    mainUi.menuHelp.triggered.connect(performMenuAction)
    # QtCore.QObject.connect(mainUi.menuHelp, QtCore.SIGNAL('triggered(QAction*)'), performMenuAction)

    #prepare the plot
    global g_curveCpuHist
    global g_curveCpuSystemHist
    global g_curveIoWaitHist
    global g_curveIrqHist
    global g_curveCpuPlotGrid

    g_curveCpuHist = plotobjects.niceCurve("CPU History",
                                                      1 , QtGui.QColor(0,255,0),QtGui.QColor(0,170,0),
                                                      mainUi.qwtPlotOverallCpuHist)

    g_curveCpuSystemHist = plotobjects.niceCurve("CPU Kernel History",
                                                      1, QtGui.QColor(255,0,0),QtGui.QColor(170,0,0),
                                                      mainUi.qwtPlotOverallCpuHist)

    g_curveIoWaitHist = plotobjects.niceCurve("CPU IO wait history",
                                                      1, QtGui.QColor(0,0,255),QtGui.QColor(0,0,127),
                                                      mainUi.qwtPlotOverallCpuHist)

    g_curveIrqHist = plotobjects.niceCurve("CPU irq history",
                                                      1, QtGui.QColor(0,255,255),QtGui.QColor(0,127,127),
                                                      mainUi.qwtPlotOverallCpuHist)

    '!!! MY'
    _cpu_hist_picker = CanvasPicker(mainUi.qwtPlotOverallCpuHist)

    scale = plotobjects.scaleObject()
    scale.min = 0
    scale.max = 100
    _ = plotobjects.procExpPlot(mainUi.qwtPlotOverallCpuHist, scale, hasGrid=False)

    ram('prepare ui />')


def clearTree():
    """ clear the tree of processes.
    """
    global g_mainUi
    global g_treeProcesses
    global g_toplevelItems
    global g_greenTopLevelItems
    global g_redTopLevelItems

    g_mainUi.processTreeWidget.clear()
    g_treeProcesses = {}
    g_toplevelItems = {}
    g_greenTopLevelItems = {}
    g_redTopLevelItems = {}

def killProcessTree(proc, procList):
    """ Kill a tree of processes, the hard way
    """
    killChildsTree(int(str(proc)), procList)
    utils.procutils.killProcessHard(int(str(proc)))

def killChildsTree(proc, procList):
    """ kill all childs of given process
    """
    for aproc in procList:
        if procList[aproc]["PPID"] == proc:
            killChildsTree(aproc, procList)
            utils.procutils.killProcess(aproc)

def addProcessAndParents(proc, procList):
    """ adds a process and its parents to the tree of processes
    """
    global g_mainUi

    if proc in g_treeProcesses.keys(): # process already exists, do nothing
        return g_treeProcesses[proc]

    g_treeProcesses[proc] = QtWidgets.QTreeWidgetItem([])
    g_greenTopLevelItems[proc] = g_treeProcesses[proc]

    if procList[proc]["PPID"] > 0 and (procList[proc]["PPID"] in procList.keys()): # process has a parent
        parent = addProcessAndParents(procList[proc]["PPID"],procList)
        parent.addChild(g_treeProcesses[proc])
    else: # process has no parent, thus it is toplevel. add it to the treewidget
        g_mainUi.processTreeWidget.addTopLevelItem(g_treeProcesses[proc])
        g_toplevelItems[proc] = g_treeProcesses[proc]

    return g_treeProcesses[proc]

def take_all_children(item, removed_items):
    if item != None:
        for i in range(item.childCount()-1, -1, -1):
            ch = item.child(i)
            if ch != None:
                take_all_children(ch, removed_items)

                removed_items.append(item.takeChild(i))

def delChild(item, childtodelete, removed_items=None):
    """ Delete child, search recursively
    """
    if item != None:
        for index in range(item.childCount()):
            thechild = item.child(index)
            if thechild != None:
                if thechild == childtodelete:
                    ch = item.takeChild(index)
                    if removed_items is not None:
                        removed_items.append(ch)
                else:
                    delChild(thechild, childtodelete)


_printed = False

def updateUI():
    """update"""
    tcpip_stat.tcpStat().tick()
    try:
        global g_procList
        global g_treeProcesses, g_greenTopLevelItems, g_redTopLevelItems
        global g_mainUi
        global g_firstUpdate

        if g_mainUi.freezeCheckBox.isChecked():
            '!!! BP'
            global _printed
            if not _printed:
                _printed = True
                g_reader.doReadProcessInfo()
                g_procList, closedProc, newProc = g_reader.getProcessInfo()
                #print(f'--- PROCS: {g_procList}\n===\n{closedProc}\n===\n{newProc}\n\n')

            bp()
            return

        g_reader.doReadProcessInfo()
        g_procList, closedProc, newProc = g_reader.getProcessInfo()


        # apply alt colors
        _apply_tree_altcolors()

        # color all green processes with default background
        defaultBgColor = app.palette().color(QtGui.QPalette.Base)
        for proc in g_greenTopLevelItems:
            for column in range(g_greenTopLevelItems[proc].columnCount()):
                g_greenTopLevelItems[proc].setBackground(column, defaultBgColor)
        g_greenTopLevelItems = {}

        #delete all red widgetItems
        for proc in g_redTopLevelItems:
            for topLevelIndex in range(g_mainUi.processTreeWidget.topLevelItemCount()):
                topLevelItem = g_mainUi.processTreeWidget.topLevelItem(topLevelIndex)
                delChild(topLevelItem, g_redTopLevelItems[proc])
                if topLevelItem == g_redTopLevelItems[proc]:
                    g_mainUi.processTreeWidget.takeTopLevelItem(topLevelIndex)

        g_redTopLevelItems = {}

        #create new items and mark items to be deleted red
        #draw tree hierarchy of processes
        for proc in newProc:
            widgetItem = addProcessAndParents(proc, g_procList)

        #if the process has childs which do still exist, "reparent" the child.
        for proc in g_procList:
            if g_procList[proc]["PPID"] == 0:
                item = g_treeProcesses[proc]
                if item.parent() is not None:
                    parentItem = item.parent()
                    for idx in range(parentItem.childCount()):
                        if item == parentItem.child(idx):
                            parentItem.takeChild(idx)
                    g_mainUi.processTreeWidget.addTopLevelItem(g_treeProcesses[proc])

        #copy processed to be deleted to the red list
        for proc in closedProc:
            try:
                g_redTopLevelItems[proc] = g_treeProcesses[proc]
            except KeyError:
                pass

        #color all deleted processed red
        for proc in g_redTopLevelItems:
            try:
                for column in range(g_redTopLevelItems[proc].columnCount()):
                    g_redTopLevelItems[proc].setBackground(column, QtGui.QColor(255,0,0))
            except RuntimeError:
                pass

        # update status information about the processes
        try:
            for proc in g_procList:
                it = g_treeProcesses[proc]
                it.setData(0, 0, g_procList[proc]["name"])
                it.setData(1, 0, str(proc))
                it.setData(2, 0, g_procList[proc]["cpuUsage"])
                it.setData(3, 0, g_procList[proc]["cmdline"])
                it.setData(4, 0, g_procList[proc]["uid"])
                it.setData(5, 0, g_procList[proc]["wchan"])
                it.setData(6, 0, g_procList[proc]["nfThreads"])

                # hints for long columns: {"Process","Command Line", "User"} 0,3,4
                it.setToolTip(0, g_procList[proc]["name"])
                it.setToolTip(3, g_procList[proc]["cmdline"])
                it.setToolTip(4, g_procList[proc]["uid"])

        except RuntimeError:
            #underlying c++ object has been deleted
            pass

        for proc in closedProc:
            try:
                del g_treeProcesses[proc]
            except KeyError:
                pass

        #color all new processes 'green'
        if g_firstUpdate == False:
            for proc in g_greenTopLevelItems:
                item = g_greenTopLevelItems[proc]
                for column in range(item.columnCount()):
                    item.setBackground(column, QtGui.QColor(0,255,0))

        #FIXME
        if (len(closedProc) > 0) or (len(newProc) > 0):
            g_mainUi.processTreeWidget.expandAll()

        for ui in g_singleProcessUiList:
            g_singleProcessUiList[ui].update()

        #update CPU plots
        g_systemOverviewUi.update()

        #network plots
        g_networkOverviewUi.update()

        #update the cpu graph
        global g_cpuUsageHistory
        global g_cpuUsageSystemHistory
        global g_cpuUsageIoWaitHistory
        global g_cpuUsageIrqHistory

        global g_curveCpuHist
        global g_curveCpuSystemHist
        global g_curveIrqHist
        global g_curveIoWaitHist
        global g_curveCpuPlotGrid

        cpu_total = g_reader.overallUserCpuUsage() + g_reader.overallSystemCpuUsage() + \
                g_reader.overallIoWaitCpuUsage() + g_reader.overallIrqCpuUsage()
        g_cpuUsageHistory.append(cpu_total)
        #g_cpuUsageHistory = g_cpuUsageHistory[1:]
        del g_cpuUsageHistory[0]

        g_cpuUsageSystemHistory.append(g_reader.overallSystemCpuUsage()+
                                                                  g_reader.overallIoWaitCpuUsage()+
                                                                  g_reader.overallIrqCpuUsage())
        #g_cpuUsageSystemHistory = g_cpuUsageSystemHistory[1:]
        del g_cpuUsageSystemHistory[0]

        g_cpuUsageIoWaitHistory.append(g_reader.overallIoWaitCpuUsage() +
                                                                  g_reader.overallIrqCpuUsage())
        #g_cpuUsageIoWaitHistory = g_cpuUsageIoWaitHistory[1:]
        del g_cpuUsageIoWaitHistory[0]

        g_cpuUsageIrqHistory.append(g_reader.overallIrqCpuUsage())
        #g_cpuUsageIrqHistory = g_cpuUsageIrqHistory[1:]
        del g_cpuUsageIrqHistory[0]

        g_curveCpuHist.setData(range(int(g_settings["historySampleCount"])), g_cpuUsageHistory)
        g_curveCpuSystemHist.setData(range(int(g_settings["historySampleCount"])), g_cpuUsageSystemHistory)
        g_curveIoWaitHist.setData(range(int(g_settings["historySampleCount"])), g_cpuUsageIoWaitHistory)
        g_curveIrqHist.setData(range(int(g_settings["historySampleCount"])), g_cpuUsageIrqHistory)
        g_mainUi.qwtPlotOverallCpuHist.replot()

        logui.update()

        #update memory figures
        mem = g_reader.getMemoryUsage()
        totalSwap = mem[5]
        actualSwap = mem[4]
        g_mainUi.memory.setValue(mem[0]-mem[1])
        g_mainUi.memory.setMaximum(mem[0])
        g_mainUi.swap.setValue(actualSwap)
        if totalSwap > 0:
            g_mainUi.swap.setMaximum(totalSwap)
        else:
            g_mainUi.swap.setMaximum(1)

        # statusbar
        sb_items = [
            f'CPU: {cpu_total:.1f}',
            f'RAM: {mem[1]/1024:.1f} / {mem[0]/1024:.1f} MB',
            f'Processes: {len(g_procList)}',
        ]
        g_mainUi.statusbar.showMessage('  ][  '.join(sb_items))

    except:
        import traceback
        utils.procutils.log("Unhandled exception:%s" %traceback.format_exc())
        print(traceback.format_exc())

    g_firstUpdate = False

if __name__ == "__main__":
    print("Call to __main__")
    app = QtWidgets.QApplication(sys.argv)
    print("app created: '%s'" % app)
    #bp()

    g_mainWindow = QtWidgets.QMainWindow()
    g_mainUi = uic.loadUi(os.path.join(os.path.dirname(__file__), "./ui/main.ui"), baseinstance=g_mainWindow)
    prepareUI(g_mainUi)
    loadSettings()

    g_mainWindow.show()
    app.processEvents()

    rootproxy.start(asRoot=False)
    if not rootproxy.isStarted():
        messageui.doMessageWindow("Process explorer has no root privileges. TCPIP traffic monitoring (using tcpdump) will not be available.")

    g_reader = procreader.reader.procreader(
            int(g_settings["updateTimer"]),
            int(g_settings["historySampleCount"]),
    )

    g_timer.start(int(g_settings["updateTimer"]))

    if g_onlyUser:
        g_reader.setFilterUID(os.geteuid())

    g_systemOverviewUi = systemoverview.systemOverviewUi(
            g_reader.getCpuCount(),
            int(g_settings["historySampleCount"]),
            g_reader,
    )
    g_networkOverviewUi = networkoverview.networkOverviewUi(
            g_reader.getNetworkCards(),
            int(g_settings["historySampleCount"]),
            g_reader,
    )

    g_systemOverviewUi.setFontSize(int(g_settings["fontSize"]))
    g_networkOverviewUi.setFontSize(int(g_settings["fontSize"]))

    updateUI()

    signal.signal(signal.SIGINT, signal.SIG_DFL)

    return_code = app.exec_()
    tcpip_stat.tcpStat().stop()
    rootproxy.end()
    sys.exit(return_code)

