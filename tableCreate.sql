DROP TABLE IF EXISTS ARTICLES;

CREATE TABLE ARTICLES(
   doi              TEXT  PRIMARY KEY UNIQUE,
   sourceID         TEXT     NOT NULL,
   sourceIDType     TEXT,
   title            TEXT,
   publicationName  TEXT,
   type             TEXT,
   volume           TEXT,
   issueIdentifier  TEXT,
   pageRange        TEXT,
   number           TEXT,
   coverDate        DATE,
   publisher        TEXT,
   pubType          TEXT,
   projectID        INT,
   pagenum          INT,
   createdon        timestamp default current_timestamp
);

DROP TABLE IF EXISTS AUTHORS;

CREATE TABLE AUTHORS(
   doi              TEXT NOT NULL,
   sourceID         TEXT NOT NULL,
   sourceIDType     TEXT,
   givenName        TEXT,
   surName          TEXT,
   index            INT,
    createdon        timestamp default current_timestamp

);

DROP TABLE IF EXISTS KEYWORDS;

CREATE TABLE KEYWORDS(
   doi              TEXT NOT NULL,
   sourceID         TEXT NOT NULL,
   sourceIDType     TEXT,
   keyword          TEXT,
   index            INT,
   createdon        timestamp default current_timestamp

);

DROP TABLE IF EXISTS EMAILS;

CREATE TABLE EMAILS(
   doi              TEXT NOT NULL,
   email            TEXT,
   givenName        TEXT,
   surName          TEXT

   CONSTRAINT proper_email CHECK (email ~* '^[A-Za-z0-9._%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$')

);

DROP TABLE IF EXISTS STATCHECK;

CREATE TABLE STATCHECK(
   doi              TEXT NOT NULL,

   statistic	    TEXT,
    df1	            INT,
    df2	            INT,
testcomparison     TEXT,	
value	            NUMERIC,
reportedcomparison	TEXT,
reportedpvalue  	numeric,
computed	        TEXT,
raw                 TEXT,
error               BOOLEAN,
decisionerror	    BOOLEAN,
onetail	            BOOLEAN,
onetailedintxt      BOOLEAN,
apafactor           numeric,
fromfulltext          BOOLEAN,
pagenum             INT
);

CREATE TABLE failedDownload(
    url TEXT
);

CREATE TABLE failedXML(
    id TEXT
);

CREATE TABLE duplicates(
   doi              TEXT  ,
   sourceID         TEXT     NOT NULL,
   sourceIDType     TEXT,
   title            TEXT,
   publicationName  TEXT,
   type             TEXT,
   volume           TEXT,
   issueIdentifier  TEXT,
   pageRange        TEXT,
   number           TEXT,
   coverDate        DATE,
   publisher        TEXT,
   pubType          TEXT,
   projectID        INT,
   createdon        timestamp default current_timestamp
);
