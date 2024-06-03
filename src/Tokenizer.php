<?php

declare(strict_types=1);

namespace Doctrine\SqlFormatter;

use function array_combine;
use function array_keys;
use function array_map;
use function array_merge;
use function arsort;
use function assert;
use function implode;
use function preg_match;
use function preg_quote;
use function str_replace;
use function strlen;
use function strpos;
use function strtoupper;
use function substr;

/** @internal */
final class Tokenizer
{
    /**
     * Reserved MySQL and MariaDB keywords.
     *
     * All these keywords must be quoted.
     *
     * @var list<string>
     */
    private array $reserved = [
        'ACCESSIBLE',
        'ADD',
        'ALL',
        'ALTER',
        'ANALYZE',
        'AND',
        'AS',
        'ASC',
        'ASENSITIVE',
        'BEFORE',
        'BETWEEN',
        'BIGINT',
        'BINARY',
        'BLOB',
        'BOTH',
        'BY',
        'CALL',
        'CASCADE',
        'CASE',
        'CHANGE',
        'CHAR',
        'CHARACTER',
        'CHECK',
        'COLLATE',
        'COLUMN',
        'CONDITION',
        'CONSTRAINT',
        'CONTINUE',
        'CONVERT',
        'CREATE',
        'CROSS',
        'CURRENT_DATE',
        'CURRENT_TIME',
        'CURRENT_TIMESTAMP',
        'CURRENT_USER',
        'CURSOR',
        'DATABASE',
        'DATABASES',
        'DAY_HOUR',
        'DAY_MICROSECOND',
        'DAY_MINUTE',
        'DAY_SECOND',
        'DEC',
        'DECIMAL',
        'DECLARE',
        'DEFAULT',
        'DELAYED',
        'DELETE',
        'DESC',
        'DESCRIBE',
        'DETERMINISTIC',
        'DISTINCT',
        'DISTINCTROW',
        'DIV',
        'DOUBLE',
        'DROP',
        'DUAL',
        'EACH',
        'ELSE',
        'ELSEIF',
        'ENCLOSED',
        'ESCAPED',
        'EXCEPT',
        'EXISTS',
        'EXIT',
        'EXPLAIN',
        'FALSE',
        'FETCH',
        'FLOAT',
        'FLOAT4',
        'FLOAT8',
        'FOR',
        'FORCE',
        'FOREIGN',
        'FROM',
        'FULLTEXT',
        'GRANT',
        'GROUP',
        'HAVING',
        'HIGH_PRIORITY',
        'HOUR_MICROSECOND',
        'HOUR_MINUTE',
        'HOUR_SECOND',
        'IF',
        'IGNORE',
        'IN',
        'INDEX',
        'INFILE',
        'INNER',
        'INOUT',
        'INSENSITIVE',
        'INSERT',
        'INT',
        'INT1',
        'INT2',
        'INT3',
        'INT4',
        'INT8',
        'INTEGER',
        'INTERSECT',
        'INTERVAL',
        'INTO',
        'IS',
        'ITERATE',
        'JOIN',
        'KEY',
        'KEYS',
        'KILL',
        'LEADING',
        'LEAVE',
        'LEFT',
        'LIKE',
        'LIMIT',
        'LINEAR',
        'LINES',
        'LOAD',
        'LOCALTIME',
        'LOCALTIMESTAMP',
        'LOCK',
        'LONG',
        'LONGBLOB',
        'LONGTEXT',
        'LOOP',
        'LOW_PRIORITY',
        'MATCH',
        'MAXVALUE',
        'MEDIUMBLOB',
        'MEDIUMINT',
        'MEDIUMTEXT',
        'MIDDLEINT',
        'MINUTE_MICROSECOND',
        'MINUTE_SECOND',
        'MOD',
        'MODIFIES',
        'NATURAL',
        'NOT',
        'NO_WRITE_TO_BINLOG',
        'NULL',
        'NUMERIC',
        'ON',
        'OPTIMIZE',
        'OPTION',
        'OPTIONALLY',
        'OR',
        'ORDER',
        'OUT',
        'OUTER',
        'OUTFILE',
        'OVER',
        'PARTITION',
        'PRECISION',
        'PRIMARY',
        'PROCEDURE',
        'PURGE',
        'RANGE',
        'READ',
        'READS',
        'READ_WRITE',
        'REAL',
        'RECURSIVE',
        'REFERENCES',
        'REGEXP',
        'RELEASE',
        'RENAME',
        'REPEAT',
        'REPLACE',
        'REQUIRE',
        'RESIGNAL',
        'RESTRICT',
        'RETURN',
        'REVOKE',
        'RIGHT',
        'RLIKE',
        'ROWS',
        'ROW_NUMBER',
        'SCHEMA',
        'SCHEMAS',
        'SECOND_MICROSECOND',
        'SELECT',
        'SENSITIVE',
        'SEPARATOR',
        'SET',
        'SHOW',
        'SIGNAL',
        'SMALLINT',
        'SPATIAL',
        'SPECIFIC',
        'SQL',
        'SQLEXCEPTION',
        'SQLSTATE',
        'SQLWARNING',
        'SQL_BIG_RESULT',
        'SQL_CALC_FOUND_ROWS',
        'SQL_SMALL_RESULT',
        'SSL',
        'STARTING',
        'STRAIGHT_JOIN',
        'TABLE',
        'TERMINATED',
        'THEN',
        'TINYBLOB',
        'TINYINT',
        'TINYTEXT',
        'TO',
        'TRAILING',
        'TRIGGER',
        'TRUE',
        'UNDO',
        'UNION',
        'UNIQUE',
        'UNLOCK',
        'UNSIGNED',
        'UPDATE',
        'USAGE',
        'USE',
        'USING',
        'UTC_DATE',
        'UTC_TIME',
        'UTC_TIMESTAMP',
        'VALUES',
        'VARBINARY',
        'VARCHAR',
        'VARCHARACTER',
        'VARYING',
        'WHEN',
        'WHERE',
        'WHILE',
        'WINDOW',
        'WITH',
        'WRITE',
        'XOR',
        'YEAR_MONTH',
        'ZEROFILL',
    ];

    /**
     * Non-reserved MySQL, MariaDB, SQLite keywords
     *
     * @var list<string>
     */
    private array $nonreserved = [
        'ABORT',
        'ACTION',
        'AFTER',
        'ALWAYS',
        'ATTACH',
        'AUTOINCREMENT',
        'AUTO_INCREMENT',
        'BEGIN',
        'CAST',
        'CHARSET',
        'COMMIT',
        'CONFLICT',
        'CUBE',
        'CUME_DIST',
        'CURRENT',
        'CURRENT_ROLE',
        'DEFERRABLE',
        'DEFERRED',
        'DELETE_DOMAIN_ID',
        'DENSE_RANK',
        'DETACH',
        'DO',
        'DO_DOMAIN_IDS',
        'EMPTY',
        'END',
        'ENGINE',
        'ENGINES',
        'ENGINE_TYPE',
        'ESCAPE',
        'EXCLUDE',
        'EXCLUSIVE',
        'FAIL',
        'FILTER',
        'FIRST',
        'FIRST_VALUE',
        'FOLLOWING',
        'FULL',
        'FUNCTION',
        'GENERAL',
        'GENERATED',
        'GET',
        'GLOB',
        'GROUPING',
        'GROUPS',
        'IGNORE_DOMAIN_IDS',
        'IGNORE_SERVER_IDS',
        'IMMEDIATE',
        'INDEXED',
        'INITIALLY',
        'INSTEAD',
        'IO_AFTER_GTIDS',
        'IO_BEFORE_GTIDS',
        'ISNULL',
        'JSON_TABLE',
        'LAG',
        'LAST',
        'LAST_VALUE',
        'LATERAL',
        'LEAD',
        'MANUAL',
        'MASTER_HEARTBEAT_PERIOD',
        'MASTER_SSL_VERIFY_SERVER_CERT',
        'MATERIALIZED',
        'MODIFY',
        'NAMES',
        'NO',
        'NOTHING',
        'NOTNULL',
        'NTH_VALUE',
        'NTILE',
        'NULLS',
        'OF',
        'OFFSET',
        'OPTIMIZER_COSTS',
        'OTHERS',
        'PAGE_CHECKSUM',
        'PARALLEL',
        'PARSE_VCOL_EXPR',
        'PERCENT_RANK',
        'PLAN',
        'PRAGMA',
        'PRECEDING',
        'QUALIFY',
        'QUERY',
        'RAISE',
        'RANK',
        'REF_SYSTEM_ID',
        'REINDEX',
        'RETURNING',
        'ROLLBACK',
        'ROW',
        'SAVEPOINT',
        'SLOW',
        'STATS_AUTO_RECALC',
        'STATS_PERSISTENT',
        'STATS_SAMPLE_PAGES',
        'STORED',
        'SYSTEM',
        'TABLES',
        'TABLESAMPLE',
        'TEMP',
        'TEMPORARY',
        'TIES',
        'TRANSACTION',
        'UNBOUNDED',
        'VACUUM',
        'VIEW',
        'VIRTUAL',
        'WITHOUT',
    ];

    /**
     * For SQL formatting
     * These keywords will all be on their own line
     *
     * @var list<string>
     */
    private array $xxreservedToplevel = [
        'ADD',
        'ALTER TABLE',
        'CHANGE',
        'DELETE FROM',
        'DROP',
        'EXCEPT',
        'FROM',
        'GROUP BY',
        'GROUPS',
        'HAVING',
        'INTERSECT',
        'LIMIT',
        'MODIFY',
        'ORDER BY',
        'PARTITION BY',
        'RANGE',
        'ROWS',
        'SELECT',
        'SET',
        'UNION',
        'UNION ALL',
        'UPDATE',
        'VALUES',
        'WHERE',
        'WINDOW',
        'WITH',
    ];

    /** @var list<string> */
    private array $reservedNewline = [
        'AND',
        'EXCLUDE',
        'INNER JOIN',
        'JOIN',
        'LEFT JOIN',
        'LEFT OUTER JOIN',
        'OR',
        'OUTER JOIN',
        'RIGHT JOIN',
        'RIGHT OUTER JOIN',
        'XOR',
    ];

    /** @var list<string> */
    private array $functions = [
        'ABS',
        'ACOS',
        'ADDDATE',
        'ADDTIME',
        'AES_DECRYPT',
        'AES_ENCRYPT',
        'APPROX_COUNT_DISTINCT',
        'AREA',
        'ASBINARY',
        'ASCII',
        'ASIN',
        'ASTEXT',
        'ATAN',
        'ATAN2',
        'AVG',
        'BDMPOLYFROMTEXT',
        'BDMPOLYFROMWKB',
        'BDPOLYFROMTEXT',
        'BDPOLYFROMWKB',
        'BENCHMARK',
        'BIN',
        'BIT_AND',
        'BIT_COUNT',
        'BIT_LENGTH',
        'BIT_OR',
        'BIT_XOR',
        'BOUNDARY',
        'BUFFER',
        'CEIL',
        'CEILING',
        'CENTROID',
        'CHARACTER_LENGTH',
        'CHAR_LENGTH',
        'CHECKSUM_AGG',
        'COALESCE',
        'COERCIBILITY',
        'COMPRESS',
        'CONCAT',
        'CONCAT_WS',
        'CONNECTION_ID',
        'CONV',
        'CONVERT_TZ',
        'CONVEXHULL',
        'COS',
        'COT',
        'COUNT',
        'COUNT_BIG',
        'CRC32',
        'CROSSES',
        'CURDATE',
        'CURTIME',
        'DATE',
        'DATEDIFF',
        'DATE_ADD',
        'DATE_DIFF',
        'DATE_FORMAT',
        'DATE_SUB',
        'DAYNAME',
        'DAYOFMONTH',
        'DAYOFWEEK',
        'DAYOFYEAR',
        'DECODE',
        'DEGREES',
        'DES_DECRYPT',
        'DES_ENCRYPT',
        'DIFFERENCE',
        'DIMENSION',
        'DISJOINT',
        'DISTANCE',
        'ELT',
        'ENCODE',
        'ENCRYPT',
        'ENDPOINT',
        'ENVELOPE',
        'EQUALS',
        'EXP',
        'EXPORT_SET',
        'EXTERIORRING',
        'EXTRACT',
        'EXTRACTVALUE',
        'FIELD',
        'FIND_IN_SET',
        'FLOOR',
        'FORMAT',
        'FOUND_ROWS',
        'FROM_DAYS',
        'FROM_UNIXTIME',
        'GEOMCOLLFROMTEXT',
        'GEOMCOLLFROMWKB',
        'GEOMETRYCOLLECTION',
        'GEOMETRYCOLLECTIONFROMTEXT',
        'GEOMETRYCOLLECTIONFROMWKB',
        'GEOMETRYFROMTEXT',
        'GEOMETRYFROMWKB',
        'GEOMETRYN',
        'GEOMETRYTYPE',
        'GEOMFROMTEXT',
        'GEOMFROMWKB',
        'GET_FORMAT',
        'GET_LOCK',
        'GLENGTH',
        'GREATEST',
        'GROUPING_ID',
        'GROUP_CONCAT',
        'GROUP_UNIQUE_USERS',
        'HEX',
        'INET_ATON',
        'INET_NTOA',
        'INSTR',
        'INTERIORRINGN',
        'INTERSECTION',
        'INTERSECTS',
        'ISCLOSED',
        'ISEMPTY',
        'ISRING',
        'ISSIMPLE',
        'IS_FREE_LOCK',
        'IS_USED_LOCK',
        'LAST_DAY',
        'LCASE',
        'LEAST',
        'LENGTH',
        'LINEFROMTEXT',
        'LINEFROMWKB',
        'LINESTRING',
        'LINESTRINGFROMTEXT',
        'LINESTRINGFROMWKB',
        'LISTAGG',
        'LN',
        'LOAD_FILE',
        'LOCATE',
        'LOG',
        'LOG10',
        'LOG2',
        'LOWER',
        'LPAD',
        'LTRIM',
        'MAKEDATE',
        'MAKETIME',
        'MAKE_SET',
        'MASTER_POS_WAIT',
        'MAX',
        'MBRCONTAINS',
        'MBRDISJOINT',
        'MBREQUAL',
        'MBRINTERSECTS',
        'MBROVERLAPS',
        'MBRTOUCHES',
        'MBRWITHIN',
        'MD5',
        'MICROSECOND',
        'MID',
        'MIN',
        'MLINEFROMTEXT',
        'MLINEFROMWKB',
        'MONTHNAME',
        'MPOINTFROMTEXT',
        'MPOINTFROMWKB',
        'MPOLYFROMTEXT',
        'MPOLYFROMWKB',
        'MULTILINESTRING',
        'MULTILINESTRINGFROMTEXT',
        'MULTILINESTRINGFROMWKB',
        'MULTIPOINT',
        'MULTIPOINTFROMTEXT',
        'MULTIPOINTFROMWKB',
        'MULTIPOLYGON',
        'MULTIPOLYGONFROMTEXT',
        'MULTIPOLYGONFROMWKB',
        'NAME_CONST',
        'NOW',
        'NULLIF',
        'NUMGEOMETRIES',
        'NUMINTERIORRINGS',
        'NUMPOINTS',
        'OCT',
        'OCTET_LENGTH',
        'OLD_PASSWORD',
        'ORD',
        'OVERLAPS',
        'PERCENTILE_CONT',
        'PERCENTILE_DISC',
        'PERIOD_ADD',
        'PERIOD_DIFF',
        'PI',
        'POINT',
        'POINTFROMTEXT',
        'POINTFROMWKB',
        'POINTN',
        'POINTONSURFACE',
        'POLYFROMTEXT',
        'POLYFROMWKB',
        'POLYGON',
        'POLYGONFROMTEXT',
        'POLYGONFROMWKB',
        'POSITION',
        'POW',
        'POWER',
        'QUARTER',
        'QUOTE',
        'RADIANS',
        'RAND',
        'RELATED',
        'RELEASE_LOCK',
        'REVERSE',
        'ROUND',
        'ROW_COUNT',
        'RPAD',
        'RTRIM',
        'SEC_TO_TIME',
        'SESSION_USER',
        'SHA',
        'SHA1',
        'SIGN',
        'SIN',
        'SLEEP',
        'SOUNDEX',
        'SPACE',
        'SQRT',
        'SRID',
        'STARTPOINT',
        'STD',
        'STDDEV',
        'STDDEV_POP',
        'STDDEV_SAMP',
        'STDEV',
        'STDEVP',
        'STRCMP',
        'STRING_AGG',
        'STR_TO_DATE',
        'SUBDATE',
        'SUBSTR',
        'SUBSTRING',
        'SUBSTRING_INDEX',
        'SUBTIME',
        'SUM',
        'SYMDIFFERENCE',
        'SYSDATE',
        'SYSTEM_USER',
        'TAN',
        'TIME',
        'TIMEDIFF',
        'TIMESTAMP',
        'TIMESTAMPADD',
        'TIMESTAMPDIFF',
        'TIME_FORMAT',
        'TIME_TO_SEC',
        'TOUCHES',
        'TO_DAYS',
        'TRIM',
        'UCASE',
        'UNCOMPRESS',
        'UNCOMPRESSED_LENGTH',
        'UNHEX',
        'UNIQUE_USERS',
        'UNIX_TIMESTAMP',
        'UPDATEXML',
        'UPPER',
        'USER',
        'UUID',
        'VAR',
        'VARIANCE',
        'VARP',
        'VAR_POP',
        'VAR_SAMP',
        'VERSION',
        'WEEK',
        'WEEKDAY',
        'WEEKOFYEAR',
        'WITHIN',
        'X',
        'Y',
        'YEAR',
        'YEARWEEK',
    ];

    // Regular expressions for tokenizing

    private readonly string $regexBoundaries;
    private readonly string $regexReserved;
    private readonly string $regexReservedNewline;
    private readonly string $regexReservedToplevel;
    private readonly string $regexFunction;

    /**
     * Punctuation that can be used as a boundary between other tokens
     *
     * @var list<string>
     */
    private array $boundaries = [
        ',',
        ';',
        '::', // PostgreSQL cast operator
        ':',
        ')',
        '(',
        '.',
        '=',
        '<',
        '>',
        '+',
        '-',
        '*',
        '/',
        '!',
        '^',
        '%',
        '|',
        '&',
        '#',
    ];

    /**
     * Stuff that only needs to be done once. Builds regular expressions and
     * sorts the reserved words.
     */
    public function __construct()
    {
        // Sort list from longest word to shortest, 3x faster than usort
        $sortByLengthFx = static function ($values) {
            $valuesMap = array_combine($values, array_map(strlen(...), $values));
            assert($valuesMap !== false);
            arsort($valuesMap);

            return array_keys($valuesMap);
        };

        // Set up regular expressions
        $this->regexBoundaries       = '(' . implode(
            '|',
            $this->quoteRegex($this->boundaries),
        ) . ')';
        $this->regexReserved         = '(' . implode(
            '|',
            $this->quoteRegex($sortByLengthFx(array_merge($this->reserved, $this->nonreserved))),
        ) . ')';
        $this->regexReservedToplevel = str_replace(' ', '\\s+', '(' . implode(
            '|',
            $this->quoteRegex($sortByLengthFx($this->reservedToplevel)),
        ) . ')');
        $this->regexReservedNewline  = str_replace(' ', '\\s+', '(' . implode(
            '|',
            $this->quoteRegex($sortByLengthFx($this->reservedNewline)),
        ) . ')');

        $this->regexFunction = '(' . implode('|', $this->quoteRegex($sortByLengthFx($this->functions))) . ')';
    }

    /**
     * Takes a SQL string and breaks it into tokens.
     * Each token is an associative array with type and value.
     *
     * @param string $string The SQL string
     */
    public function tokenize(string $string): Cursor
    {
        $tokens = [];

        // Used to make sure the string keeps shrinking on each iteration
        $oldStringLen = strlen($string) + 1;

        $token = null;

        $currentLength = strlen($string);

        // Keep processing the string until it is empty
        while ($currentLength) {
            // If the string stopped shrinking, there was a problem
            if ($oldStringLen <= $currentLength) {
                $tokens[] = new Token(Token::TOKEN_TYPE_ERROR, $string);

                return new Cursor($tokens);
            }

            $oldStringLen =  $currentLength;

            // Get the next token and the token type
            $token       = $this->createNextToken($string, $token);
            $tokenLength = strlen($token->value());

            $tokens[] = $token;

            // Advance the string
            $string = substr($string, $tokenLength);

            $currentLength -= $tokenLength;
        }

        return new Cursor($tokens);
    }

    /**
     * Return the next token and token type in a SQL string.
     * Quoted strings, comments, reserved words, whitespace, and punctuation
     * are all their own tokens.
     *
     * @param string     $string   The SQL string
     * @param Token|null $previous The result of the previous createNextToken() call
     *
     * @return Token An associative array containing the type and value of the token.
     */
    private function createNextToken(string $string, Token|null $previous = null): Token
    {
        $matches = [];
        // Whitespace
        if (preg_match('/^\s+/', $string, $matches)) {
            return new Token(Token::TOKEN_TYPE_WHITESPACE, $matches[0]);
        }

        // Comment
        if (
            $string[0] === '#' ||
            (isset($string[1]) && ($string[0] === '-' && $string[1] === '-') ||
            (isset($string[1]) && $string[0] === '/' && $string[1] === '*'))
        ) {
            // Comment until end of line
            if ($string[0] === '-' || $string[0] === '#') {
                $last = strpos($string, "\n");
                $type = Token::TOKEN_TYPE_COMMENT;
            } else { // Comment until closing comment tag
                $pos  = strpos($string, '*/', 2);
                $last = $pos !== false
                    ? $pos + 2
                    : false;
                $type = Token::TOKEN_TYPE_BLOCK_COMMENT;
            }

            if ($last === false) {
                $last = strlen($string);
            }

            return new Token($type, substr($string, 0, $last));
        }

        // Quoted String
        if ($string[0] === '"' || $string[0] === '\'' || $string[0] === '`' || $string[0] === '[') {
            return new Token(
                ($string[0] === '`' || $string[0] === '['
                    ? Token::TOKEN_TYPE_BACKTICK_QUOTE
                    : Token::TOKEN_TYPE_QUOTE),
                $this->getQuotedString($string),
            );
        }

        // User-defined Variable
        if (($string[0] === '@' || $string[0] === ':') && isset($string[1])) {
            $value = null;
            $type  = Token::TOKEN_TYPE_VARIABLE;

            // If the variable name is quoted
            if ($string[1] === '"' || $string[1] === '\'' || $string[1] === '`') {
                $value = $string[0] . $this->getQuotedString(substr($string, 1));
            } else {
                // Non-quoted variable name
                preg_match('/^(' . $string[0] . '[a-zA-Z0-9\._\$]+)/', $string, $matches);
                if ($matches) {
                    $value = $matches[1];
                }
            }

            if ($value !== null) {
                return new Token($type, $value);
            }
        }

        // Number (decimal, binary, or hex)
        if (
            preg_match(
                '/^([0-9]+(\.[0-9]+)?|0x[0-9a-fA-F]+|0b[01]+)($|\s|"\'`|' . $this->regexBoundaries . ')/',
                $string,
                $matches,
            )
        ) {
            return new Token(Token::TOKEN_TYPE_NUMBER, $matches[1]);
        }

        // Boundary Character (punctuation and symbols)
        if (preg_match('/^(' . $this->regexBoundaries . ')/', $string, $matches)) {
            return new Token(Token::TOKEN_TYPE_BOUNDARY, $matches[1]);
        }

        // A reserved word cannot be preceded by a '.'
        // this makes it so in "mytable.from", "from" is not considered a reserved word
        if (! $previous || $previous->value() !== '.') {
            $upper = strtoupper($string);
            // Top Level Reserved Word
            if (
                preg_match(
                    '/^(' . $this->regexReservedToplevel . ')($|\s|' . $this->regexBoundaries . ')/',
                    $upper,
                    $matches,
                )
            ) {
                return new Token(
                    Token::TOKEN_TYPE_RESERVED_TOPLEVEL,
                    substr($string, 0, strlen($matches[1])),
                );
            }

            // Newline Reserved Word
            if (
                preg_match(
                    '/^(' . $this->regexReservedNewline . ')($|\s|' . $this->regexBoundaries . ')/',
                    $upper,
                    $matches,
                )
            ) {
                return new Token(
                    Token::TOKEN_TYPE_RESERVED_NEWLINE,
                    substr($string, 0, strlen($matches[1])),
                );
            }

            // Other Reserved Word
            if (
                preg_match(
                    '/^(' . $this->regexReserved . ')($|\s|' . $this->regexBoundaries . ')/',
                    $upper,
                    $matches,
                )
            ) {
                return new Token(
                    Token::TOKEN_TYPE_RESERVED,
                    substr($string, 0, strlen($matches[1])),
                );
            }
        }

        // A function must be succeeded by '('
        // this makes it so "count(" is considered a function, but "count" alone is not
        $upper = strtoupper($string);
        // function
        if (preg_match('/^(' . $this->regexFunction . '[(]|\s|[)])/', $upper, $matches)) {
            return new Token(
                Token::TOKEN_TYPE_RESERVED,
                substr($string, 0, strlen($matches[1]) - 1),
            );
        }

        // Non reserved word
        preg_match('/^(.*?)($|\s|["\'`]|' . $this->regexBoundaries . ')/', $string, $matches);

        return new Token(Token::TOKEN_TYPE_WORD, $matches[1]);
    }

    /**
     * Helper function for building regular expressions for reserved words and boundary characters
     *
     * @param string[] $strings The strings to be quoted
     *
     * @return string[] The quoted strings
     */
    private function quoteRegex(array $strings): array
    {
        return array_map(
            static fn (string $string): string => preg_quote($string, '/'),
            $strings,
        );
    }

    private function getQuotedString(string $string): string
    {
        $ret = '';

        // This checks for the following patterns:
        // 1. backtick quoted string using `` to escape
        // 2. square bracket quoted string (SQL Server) using ]] to escape
        // 3. double quoted string using "" or \" to escape
        // 4. single quoted string using '' or \' to escape
        if (
            preg_match(
                '/^(((`[^`]*($|`))+)|
            ((\[[^\]]*($|\]))(\][^\]]*($|\]))*)|
            (("[^"\\\\]*(?:\\\\.[^"\\\\]*)*("|$))+)|
            ((\'[^\'\\\\]*(?:\\\\.[^\'\\\\]*)*(\'|$))+))/sx',
                $string,
                $matches,
            )
        ) {
            $ret = $matches[1];
        }

        return $ret;
    }
}
