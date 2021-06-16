#this file is required to satisfy direct .ui file loading in process explorer
#as e.g. systeminformation.ui imports qwt_plot module.
#from PyQt5.Qwt import QwtPlot
from PyQt5 import QtCore

from qwt import QwtPlot

class QwtPlotFixed(QwtPlot):

    def event(self, event):
        if event.type() == QtCore.QEvent.PolishRequest:
            # stop event: causes 100% cpu usage
            event.accept()
            return True
        return QtCore.QObject.event(self, event)
