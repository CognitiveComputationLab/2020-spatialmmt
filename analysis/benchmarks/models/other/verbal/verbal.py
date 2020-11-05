""" Implements Spatial Reasoning as Verbal Reasoning, with cardinal directions.

"""


import ccobra
import random


CLASSES = {"north": {"north", "north-east", "north-west"},
           "north-east": {"north-east", "north", "east"},
           "east": {"east", "north-east", "south-east"},
           "south-east": {"south-east", "south", "east"},
           "south": {"south", "south-east", "south-west"},
           "south-west": {"south-west", "south", "west"},
           "west": {"west", "south-west", "north-west"},
           "north-west": {"north-west", "north", "west"},
           "left": {"left"},
           "right": {"right"}}

OPPOSITES = {"north": {"south", "south-east", "south-west"},
             "north-east": {"south-west", "south", "west"},
             "east": {"west", "north-west", "south-west"},
             "south-east": {"north-west", "north", "west"},
             "south": {"north", "north-east", "north-west"},
             "south-west": {"north-east", "north", "east"},
             "west": {"east", "south-east", "north-east"},
             "north-west": {"south-east", "south", "east"},
             "left": {"right"},
             "right": {"left"}}

ORTHOGONAL = {"north": {"east", "west"},
              "north-east": {"south-east", "north-west"},
              "east": {"north", "south"},
              "south-east": {"north-east", "south-west"},
              "south": {"east", "west"},
              "south-west": {"south-east", "north-west"},
              "west": {"north", "south"},
              "north-west": {"north-east", "south-west"},
              "left": {},
              "right": {}}

POLAR_OPPOSITES = {"north": "south",
                   "north-east": "south-west",
                   "east": "west",
                   "south-east": "north-west",
                   "south": "north",
                   "south-west": "north-east",
                   "west": "east",
                   "north-west": "south-east",
                   "left": "right",
                   "right": "left"}


class QueueItem():
    """ A single item of the mental queue constructed by the virtual reasoner.
    """

    def __init__(self, data):
        """ Initialize the item.

        Parameters
        ----------
        data : str
            The object data contained by the queue item.

        """
        self.data = data
        self.next = None
        self.northsouth = 0
        self.eastwest = 0


class Queue():
    """ The mental queue constructed by the virtual reasoner.
    """

    def __init__(self, dimension):
        """ Initialize the queue.

        Parameters
        ----------
        dimension : intDTF
            Specify whether one- or two-dimensional queue should be
            constructed.

        """
        self.dimension = dimension
        self.origin = None
        self.first = None

    def insert_first(self, new_item, northsouth=0, eastwest=0):
        """ Insers a item at the start of the queue.

        Parameters
        ----------
        new_item : QueueItem
            The item to be inserted.

        """
        new_item.next = self.first
        self.first = new_item
        self.first.northsouth = northsouth
        self.first.eastwest = eastwest

    def insert_after(self, item, new_item, northsouth=0, eastwest=0):
        """ Insers an item behind another item into the queue.

        Parameters
        ----------
        item : QueueItem
            The reference item.
        new_item : QueueItem
            The item to be inserted.

        """
        new_item.next = item.next
        if new_item.next is not None:
            new_item.northsouth = item.northsouth
            new_item.eastwest = item.eastwest
        item.next = new_item
        item.northsouth = northsouth
        item.eastwest = eastwest

    def insert_before(self, current, previous, new_item, northsouth=0,
                      eastwest=0):
        """ Inserts an item before its
        reference item into the queue.

        Parameters
        ----------
        current : QueueItem
            The reference item.
        previous : QueueItem
            The item preceding the reference item.
        new_item : QueueItem
            The item to be inserted.

        """
        if current == self.first:
            self.insert_first(new_item, -northsouth, -eastwest)
        else:
            prev_northsouth = previous.northsouth + northsouth
            prev_eastwest = previous.eastwest + eastwest
            if prev_northsouth == 0 and prev_eastwest == 0:
                default = self.get_direction_encoding(self.origin)
                prev_northsouth = -default[0]
                prev_eastwest = -default[1]
            previous.northsouth = -northsouth
            previous.eastwest = -eastwest
            self.insert_after(previous, new_item, prev_northsouth,
                              prev_eastwest)

    def insert_last(self, current, new_item, northsouth=0, eastwest=0):
        """ Helper for the insert function, to insert an item at the end of the
        queue.

        Parameters
        ----------
        current : QueueItem
            The reference item.
        new_item : QueueItem
            The item to be inserted.

        """
        while current.next is not None:
            current = current.next
        self.insert_after(current, new_item, northsouth, eastwest)

    def reverse(self):
        """Reverse queue.

        """


        items = []
        current = self.first
        while current is not None:
            items.append(current)
            current = current.next
        self.first = QueueItem(items[-1].data)
        current = self.first
        for i in reversed(range(len(items) - 1)):
            self.insert_after(current, QueueItem(items[i].data),
                              items[i+1].northsouth, items[i+1].eastwest)
            current = current.next

    def get_direction_encoding(self, direction_string):
        """ Returns proper encoding as tuple for a given cardinal direction
        string.

        Parameters
        ----------
        direction_string : string
            The cardinal direction.

        """
        northsouth = 0
        eastwest = 0
        if "north" in direction_string.lower():
            northsouth = 1
        elif "south" in direction_string.lower():
            northsouth = -1
        if "east" in direction_string.lower():
            eastwest = 1
        elif "west" in direction_string.lower():
            eastwest = -1
        return northsouth, eastwest

    def find_reference(self, relation):
        """ Find the reference item for ab item to be inserted.

        Parameters
        ----------
        relation : list
            List that contains the direction as string, as well as the two
            reference object strings, of the relation.

        """
        previous = None
        current = self.first
        while current.data not in relation:
            if current.next is None:
                return None, None
            previous = current
            current = current.next
        return previous, current

    def get_relative_coordinates(self, relation):
        """ Find the relative cardinal coordinates for two points within the
        queue. Return false if points do not exist in queue.

        Parameters
        ----------
        relation : list
            List that contains the direction as string, as well as the two
            reference object strings, of the relation.

        """
        first = self.first
        while first.data not in relation:
            first = first.next
            if first is None:
                return False
        northsouth = first.northsouth
        eastwest = first.eastwest
        second = first.next
        if second is None:
            return False
        while second.data not in relation:
            northsouth += second.northsouth
            eastwest += second.eastwest
            second = second.next
            if second is None:
                return False
        return first, second, northsouth, eastwest

    def compare_coordinates(self, northsouth1, eastwest1, northsouth2,
                            eastwest2):
        """ Check if two sets of coordinates face in the same direction.

        Parameters
        ----------
        northsouth1 : int
            The vertical component of the first coordinate.
        eastwest1 : int
            The horizontal component of the first coordinate.
        northsouth2 : int
            The vertical component of the first coordinate.
        eastwest2 : int
            The horizontal component of the first coordinate.
        """
        vertical1 = northsouth1 > 0 and northsouth2 > 0
        vertical2 = northsouth1 == 0 and northsouth2 == 0
        vertical3 = northsouth1 < 0 and northsouth2 < 0
        vertical4 = northsouth1 > 0 and northsouth2 > 0 and eastwest1 == 0
        vertical5 = northsouth1 < 0 and northsouth2 < 0 and eastwest1 == 0
        horizontal1 = eastwest1 > 0 and eastwest2 > 0
        horizontal2 = eastwest1 == 0 and eastwest2 == 0
        horizontal3 = eastwest1 < 0 and eastwest2 < 0
        horizontal4 = northsouth1 == 0 and eastwest1 > 0 and eastwest2 > 0
        horizontal5 = northsouth1 == 0 and eastwest1 < 0 and eastwest2 < 0
        return (vertical1 or vertical2 or vertical3 or vertical4 or vertical5) \
               and (horizontal1 or horizontal2 or horizontal3 or horizontal4 \
                    or horizontal5)

    def decide_insert_position(self, current, relation):
        """ Return true if inserting before reference object, false if
        inserting at end of queue.

        Parameters
        ----------
        current : QueueItem
            The reference item.
        relation : list
            List that contains the direction as string, as well as the two
            reference object strings, of the first relation.

        """
        if current.data == relation[1]:
            if (relation[0].lower() not in CLASSES[self.origin.lower()]) and \
                (relation[0].lower() not in ORTHOGONAL[self.origin.lower()]):
                return True
            else:
                return False
        elif current.data == relation[2]:
            if relation[0].lower() in CLASSES[self.origin.lower()]:
                return True
            else:
                return False
        else:
            return False

    def initialize_queue(self, relation, origin):
        """ Add first relation to the queue.

        Parameters
        ----------
        relation : list
            List that contains the direction as string, as well as the two
            reference object strings, of the first relation.
        origin : string
            One of the cardinal directions or one of "left" and "right",
            depending on dimension of queue. Decides how the queue should be
            oriented.

        """
        self.origin = origin
        dir_encoded = self.get_direction_encoding(relation[0])
        if origin == relation[0]:
            self.insert_first(QueueItem(relation[1]))
            self.insert_after(self.first, QueueItem(relation[2]),
                              -dir_encoded[0], -dir_encoded[1])
        else:
            self.insert_first(QueueItem(relation[2]))
            self.insert_after(self.first, QueueItem(relation[1]),
                              dir_encoded[0], dir_encoded[1])

    def can_add(self, relation):
        """ Check if relation can be added to the queue.

        Parameters
        ----------
        relation : list
            List that contains the direction as string, as well as the two
            reference object strings, of the relation.

        """
        if self.first is None:
            return True
        previous, current = self.find_reference(relation)
        return False if current is None else True

    def add_to_queue(self, relation):
        """ Adds a single relation into the queue.

        Parameters
        ----------
        relation : list
            List that contains the direction as string, as well as the two
            reference object strings, of the relation.

        """
        previous, current = self.find_reference(relation)
        new_data = relation[1] if current.data == relation[2] else \
                   relation[2]
        dir_encoded = self.get_direction_encoding(relation[0])
        if current.data == relation[2]:
            dir_encoded = (-dir_encoded[0], -dir_encoded[1])
        if self.decide_insert_position(current, relation):
            self.insert_before(current, previous, QueueItem(new_data),
                               -dir_encoded[0], -dir_encoded[1])
        else:
            self.insert_last(current, QueueItem(new_data), -dir_encoded[0],
                             -dir_encoded[1])

    def find_connection(self, other_queue):
        """ Returns a tuple of reference objects connecting the queue with
        another queue, if exists.

        Parameters
        ----------
        other_queue : Queue
            The queue that should be connected to this queue.

        """
        previous = None
        current = self.first
        while current is not None:
            other = other_queue.first
            while other is not None:
                if current.data == other.data:
                    return previous, current, other
                other = other.next
            previous = current
            current = current.next

    def can_fuse(self, other_queue):
        """ Check if already existing queue can be fused into the queue.

        Parameters
        ----------
        other_queue : Queue
            The queue that should be checked for fusion into this queue.

        """
        return True if self.find_connection(other_queue) is not None else False

    def fuse_one_dimension(self, item, previous, other_item, other_queue):
        """ Helper function to fuse two longer than 2-item one-dimensional
        queues.

        Parameters
        ----------
        item : QueueItem
            The reference item.
        previous : QueueItem
            The item preceding the reference item.
        other_item : QueueItem
            The counterpart to the reference item in the queue to be fused.
        other_queue : Queue
            The queue that should be fused into this queue.

        """
        current = other_queue.first
        while current is not other_item:
            self.insert_before(item, previous, QueueItem(current.data))
            current = current.next
        current = other_item.next
        while current is not None:
            self.insert_last(item, QueueItem(current.data))
            current = current.next

    def fuse_queues(self, other_queue):
        """ Add an already existing queue into the queue. If queue is
        two-dimensional, only 2-item-queues can be fused into an existing queue
        this way.

        Parameters
        ----------
        other_queue : Queue
            The queue that should be fused into this queue.

        """
        if other_queue.origin.lower() in OPPOSITES[self.origin.lower()]:
            other_queue.reverse()
        previous, current, other = self.find_connection(other_queue)
        if self.dimension == 1 and other_queue.dimension == 1:
            self.fuse_one_dimension(current, previous, other, other_queue)
        elif self.dimension == 2 and other_queue.dimension == 2:
            if other == other_queue.first:
                self.insert_last(other.next, other.northsouth, other.eastwest)
            else:
                self.insert_before(current, previous, other_queue.first,
                                       other.northsouth, other.eastwest)

    def validate(self, relation):
        """ Validates if a relation holds with the current state of the queue.

        Parameters
        ----------
        relation : list
            List that contains the direction as string, as well as the two
            reference object strings, of the relation.

        """
        coordinates = self.get_relative_coordinates(relation)
        if not coordinates:
            return False
        if self.dimension == 1:
            if (self.origin.lower() == relation[0].lower() and \
                coordinates[0].data == relation[1]) or \
                (self.origin.lower() != relation[0].lower() and \
                coordinates[0].data == relation[2]):
                return True
            else:
                return False
        if self.dimension == 2:
            if coordinates[0].data == relation[1]:
                encoding = self.get_direction_encoding(relation[0])
            elif coordinates[0].data == relation[2]:
                encoding = self.get_direction_encoding(
                    POLAR_OPPOSITES[relation[0].lower()])
            return self.compare_coordinates(-encoding[0], -encoding[1],
                                            coordinates[2], coordinates[3])

    def __str__(self):
        """Convert queue to string.

        """
        if self.dimension == 1:
            string = ""
            current = self.first
            if self.origin.lower() == "left":
                while current is not None:
                    string = string + current.data
                    if current.next is not None:
                        string = string + " > "
                    current = current.next
            else:
                while current is not None:
                    string = current.data + string
                    if current.next is not None:
                        string = " < " + string
                    current = current.next
        if self.dimension == 2:
            string = ""
            current = self.first
            while current is not None:
                string = string + current.data + " " + str(current.northsouth) \
                         + " " + str(current.eastwest) + "\n"
                current = current.next
        return string


class VerbalModel(ccobra.CCobraModel):
    """ Model producing responses according to the verbal models theory of
    spatial reasoning.

    """

    def __init__(self, name='Verbal Reasoning\n(Krumnack et al., 2010)'):
        """ Initializes the model.

        Parameters
        ----------
        name : str
            Unique name of the model.

        """
        super(VerbalModel, self).__init__(
            name, ["spatial-relational"], ["verify", "single-choice"])

        self.preferences = {"north": 0, "south": 0,"east": 0,"west": 0,
                            "left": 0, "right": 0, "north-reverse": 0,
                            "south-reverse": 0, "east-reverse": 0,
                            "west-reverse": 0, "left-reverse": 0,
                            "right-reverse": 0}

    def construct_queue(self, task, force_origin=0):
        """ Construct a queue from given task.

        Parameters
        ----------
        task : list
            Task containing queue relations.

        """
        dimension = 1 if task[0][0].lower() == "left" or \
                    task[0][0].lower() == "right" \
                    else 2
        queues = [Queue(dimension)]
        origin = self.decide_origin(task[0], dimension, force_origin)
        queues[0].initialize_queue(task[0], origin)
        for relation in task[1:]:
            added = False
            for queue in queues:
                if queue.can_add(relation) and (queue.dimension == 1 or \
                   (queue.dimension == 2 and queues.index(queue) == 0)):
                    queue.add_to_queue(relation)
                    added = True
                    break
            if not added:
                queues.append(Queue(dimension))
                origin = self.decide_origin(relation, dimension, force_origin)
                queues[-1].initialize_queue(relation, origin)

            queues = self.fuse_all_queues(queues)
        while len(queues) > 1:
            queues = self.fuse_all_queues(queues)
        return queues[0]

    def decide_origin(self, relation, dimension, force_origin=0):
        """ Decides origin direction of queue based on given starting relation.

        Parameters
        ----------
        relation : list
            The starting relation.

        """
        normal = relation[0]
        reverse = POLAR_OPPOSITES[relation[0].lower()]
        if force_origin == 1:
            return normal
        elif force_origin == 2:
            return reverse
        else:
            normal_cnt = 0
            reverse_cnt = 0
            for i in normal.lower().split("-"):
                normal_cnt += self.preferences[i]
            for i in reverse.lower().split("-"):
                reverse_cnt += self.preferences[i + "-reverse"]
            return normal if normal_cnt >= reverse_cnt else reverse

    def fuse_all_queues(self, queues):
        """ A single round of fusing all queues, with queues with a lower index
        as a priority.

        Parameters
        ----------
        queues : list
            A list of all queues to be fused.

        """

        new_queues = []
        fuse_subj = [False] * len(queues)
        fuse_obj = [False] * len(queues)
        for i in range(len(queues)):
            for j in range(i + 1, len(queues)):
                if queues[i].can_fuse(queues[j]) and not (fuse_obj[i] or fuse_obj[j]):
                    queues[i].fuse_queues(queues[j])
                    new_queues.append(queues[i])
                    fuse_subj[i] = True
                    fuse_obj[j] = True
            if not (fuse_subj[i] or fuse_obj[i]):
                new_queues.append(queues[i])

        return new_queues

    def compete(self, item, response):
        """ Let a normal and reversed queue compete over an item. Adjust
        preferences according to response.

        Parameters
        ----------
        item : list
            Task to produce a response for.
        response: list
            Response data for given task.

        """

        q_normal = self.construct_queue(item.task, 1)
        q_reverse = self.construct_queue(item.task, 2)
        if item.response_type == "verify":
            if q_normal.validate(item.choices[0][0]) == response:
                for i in q_normal.origin.lower().split("-"):
                    self.preferences[i] += 1
            if q_reverse.validate(item.choices[0][0]) == response:
                for i in q_reverse.origin.lower().split("-"):
                    self.preferences[i + "-reverse"] += 1
        if item.response_type == "single-choice":
            valid_normal = []
            valid_reverse = []
            for i in item.choices:
                if q_normal.validate(i[0]):
                    valid_normal.append(i[0])
                if q_reverse.validate(i[0]):
                    valid_reverse.append(i[0])
            if len(valid_normal) > 0:
                if random.choice(valid_normal) == response[0]:
                    for i in q_normal.origin.lower().split("-"):
                        self.preferences[i] += 1
            if len(valid_reverse) > 0:
                if random.choice(valid_reverse) == response[0]:
                    for i in q_reverse.origin.lower().split("-"):
                        self.preferences[i + "-reverse"] += 1

    def pre_train(self, dataset):
        """ Pretrains the model for a given dataset.

        Parameters
        ----------
        dataset : list
            Pairs of items and responses that the model should be fitted
            against.

        """
        for person_data in dataset:
            for item_data in person_data:
                item = item_data["item"]
                response = item_data["response"]
##                self.compete(item, response)

    def predict(self, item, **kwargs):
        """ Verifies a given answer or chooses valid option from set of given
        answers.

        Parameters
        ----------
        item : list
            Task to produce a response for.

        """

        q = self.construct_queue(item.task)
        if item.response_type == "verify":
            return q.validate(item.choices[0][0])
        if item.response_type == "single-choice":
            valid = []
            for i in item.choices:
                if q.validate(i[0]):
                    valid.append(i[0])
            return random.choice(valid) if len(valid) > 0 else None

    def adapt(self, item, response, **kwargs):
        """ Adapts the model to make given response likelier for given item.

        Parameters
        ----------
        item : list
            Task to produce a response for.
        response: list
            Response data for given task.

        """

        self.compete(item, response)

    def end_participant(self, subj_id, model_log, **kwargs):
        print('Participant {} done.'.format(subj_id))

