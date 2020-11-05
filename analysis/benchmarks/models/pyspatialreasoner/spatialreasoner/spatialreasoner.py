import os
import logging
import queue
import subprocess
import threading

from . import ccl as cclmod


# Term names
TERMS = [
    'square',
    'triangle',
    'circle',
    'line',
    'cross',
    'ell',
    'vee',
    'star',
    'ess',
]

class SpatialReasoner():
    def __init__(self, ccl):
        self.logger = logging.getLogger(__name__)

        self.ccl = ccl
        self.exec_path = ccl.exec_path()

        # Initialize spatialreasoner
        self.initialize_spatialreasoner()

    def initialize_spatialreasoner(self):
        # Instantiate the result queue
        self.resp_queue = queue.Queue()

        # Start the LISP process
        self.proc = subprocess.Popen(
            [self.exec_path],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT
        )

        # Register the readers
        def stdout_reader(proc):
            # Setup thread logger
            logger = logging.getLogger(__name__ + '-reader')
            logger.debug('Starting reader...')

            queue_buffer = []

            while True:
                # Read text
                text = proc.stdout.readline().decode('ascii').strip()
                logger.debug('spatialreasoner:%s', text)

                if 'TERMINATE' in text:
                    logger.debug('termination handling initiated...')
                    break

                if text == '> Type :? for other options.':
                    logger.debug('error encountered. popping...')
                    self.resp_queue.put('ERROR')
                    break

                if text == 'PREMISE  FOLLOWS  VALIDLY  FROM  PREVIOUS  PREMISES.':
                    logger.debug('validity detected.')
                    queue_buffer.append('true')

                if text == 'PREMISE  IS  INCONSISTENT  WITH  PREVIOUS  PREMISES.':
                    logger.debug('invalidity detected.')
                    queue_buffer.append('false')

                if text == 'PREMISE  WAS  PREVIOUSLY  POSSIBLY  TRUE.':
                    logger.debug('indeterminate true detected.')
                    queue_buffer.append('indeterminate-true')

                if text == 'PREMISE  WAS  PREVIOUSLY  POSSIBLY  FALSE.':
                    logger.debug('indeterminate false detected.')
                    queue_buffer.append('indeterminate-false')

                if text == '"SYNC"':
                    logger.debug("SYNC detected with queue buffer:%s", queue_buffer)
                    if queue_buffer:
                        self.resp_queue.put(queue_buffer)
                        queue_buffer = []

        self.readerstdout = threading.Thread(target=stdout_reader, args=(self.proc,), daemon=True)
        self.readerstdout.start()

        # Create the FASL file if not existent
        lisp_dir = os.path.abspath(os.path.split(os.path.abspath(__file__))[0] + '/lisp')
        lisp_path = os.path.abspath(lisp_dir + '/spatial.lisp')
        fasl_path = os.path.abspath(lisp_dir + '/spatial.{}'.format(cclmod.FSL_ENDINGS[self.ccl.system]))
        fasl_path = fasl_path.replace('\\', '\\\\')

        if not os.path.isfile(fasl_path):
            self.logger.debug('compiling the lisp code...')
            spatial_reasoner_file = lisp_path.replace('\\', '\\\\')
            self._send('(compile-file "{}")'.format(spatial_reasoner_file))

        # Load spatialreasoner
        logging.debug('loading spatialreasoner fasl...')
        self._send('(load "{}")'.format(fasl_path))

    def _send(self, cmd, send_sync=False):
        """ Send a command to the Clozure Common LISP subprocess.

        Parameters
        ----------
        cmd : str
            Command to send.

        """

        # Normalize the command
        cmd = cmd.strip()

        self.logger.debug('Send:%s', cmd)
        self.proc.stdin.write('{}\n'.format(cmd).encode('ascii'))
        self.proc.stdin.flush()

        if send_sync:
            self.logger.debug('Send:SYNC')
            self.proc.stdin.write('{}\n'.format('(prin1 "SYNC")').encode('ascii'))
            self.proc.stdin.flush()

    def terminate(self):
        """ Terminate mReasoner and its parent instance of Clozure Common LISP.

        """

        # Shutdown the threads
        self._send('(prin1 "TERMINATE")')
        self.logger.debug('Waiting for stdout...')
        self.readerstdout.join()

        # Terminate Clozure
        self._send('(quit)')

    def query(self, problem):
        self.logger.debug('Querying for problem "%s"', problem)

        # Prepare the command to send
        premises = ''.join(['({})'.format(x) for x in problem])
        cmd = "(interpret '({}))".format(premises)
        self.logger.debug('cmd: "%s"', cmd)

        # Send the command
        self._send(cmd, send_sync=True)

        # Wait for the response
        resp = self.resp_queue.get()

        if resp == 'ERROR':
            self.logger.info('Error detected. Restarting...')
            self.proc.terminate()
            self.initialize_spatialreasoner()
            return ['false']

        return resp
