from sqlalchemy import Column, Integer, Text, String, Date, ForeignKey
from sqlalchemy.orm import relationship, backref
from sqlalchemy.ext.declarative import declarative_base

Base = declarative_base()

class Personality(Base):
    __tablename__ = 'personality'

    id = Column(Integer, primary_key=True)
    name = Column(String)
    affiliation = Column(String)

    def __init__(self, name, affiliation=None):
        self.name = name
        self.affiliation = affiliation

    def __repr__(self):
        return "<Personality('{}' - '{}')>".format(self.name, self.affiliation)

class Statement(Base):
    '''claim, truthiness, personality, date
    '''
    __tablename__ = 'statement'

    id = Column(Integer, primary_key=True)
    claim = Column(Text)
    truthiness = Column(Integer) # mapped from 0 to 6, 0 = lie, 6=truth
    date = Column(Date)
    personality_id = Column(Integer, ForeignKey('personality.id'))
    personality = relationship('Personality', 
            backref = backref('statements', lazy='dynamic'))

    def __init__(self, claim, truthiness, personality, date):
        self.claim = claim
        self.truthiness = truthiness
        self.personality = personality
        self.date = date

    def __repr__(self):
        return "<Statement('{}','{}', '{}')\
                    >".format(self.claim, self.truthiness, self.personality)
