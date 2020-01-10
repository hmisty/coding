package main

import (
	"database/sql"
	"fmt"
	_ "github.com/lib/pq"
	"github.com/lib/pq/hstore"
	"strconv"
	"strings"
)

func panicIfError(err error) {
	if err != nil {
		panic(err)
	}
}

type Posting struct {
	article_id int
	in_title   bool
	count      int
}

/*
func parseArray(data []uint8) []*Posting {
	//{240973,0:1:707,241346,"0:3:1236,1251,1279"}
	var postinglist []*Posting
	var article_id int
	var in_title bool
	var count int
	var parsingK, quote, parsingV1, parsingV2, parsingV3 bool
	parsingK = true
	for i := 0; i < len(data); i++ {
		if data[i] == '{' || data[i] == '}' {
			continue
		}
		if parsingK {
			if data[i] != ',' {
				val := data[i] - '0'
				article_id = article_id * 10 + int(val)
			} else {
				parsingK = false
				parsingV1 = true
			}
		}
		if parsingV1 {
			if data[i] != ':' {
				if data[i] == '"' {
					quote = true
					continue
				} else if data[i] == '0' {
					in_title = false
				} else {
					in_title = true
				}
			} else {
				parsingV1 = false
				parsingV2 = true
			}
		}
		if parsingV2 {
			if data[i] != ':' {
				val := data[i] - '0'
				count = count * 10 + int(val)
			} else {
				parsingV2 = false
				parsingV3 = true
			}
		}
		if parsingV3 {
			if data[i] != ',' {
				continue
			} else {
				if quote && data[i] != '"' {
					continue
				} else {
					parsingV3 = false
					parsingK = true

					p := Posting{article_id: article_id, in_title: in_title, count: count}
					postinglist = append(postinglist, &p)
					article_id = 0
					in_title = false
					count = 0
				}
			}
		}
	}

	return postinglist
}
*/

func main() {
	db, err := sql.Open("postgres", "host=10.146.240.113 port=3433 user=newcms password=feedflow dbname=newcms sslmode=disable")
	panicIfError(err)

	keywords := []string{"肠胃","奶类","流产","纤维素类","泻药","胃肠","泻剂","调理","刺激性","不良反应","蔬菜类","肠道","黑枣","保健品","用药","纤维","枣子","全麦","买药","致突变作用"}
	keyword_str := "'" + strings.Join(keywords, "','") + "'"
	//rows, err := db.Query("SELECT id, word, hstore_to_array(documents) FROM search_searchindex WHERE word IN (" + keyword_str + ")")
	rows, err := db.Query("SELECT id, word, documents FROM search_searchindex WHERE word IN (" + keyword_str + ")")
	panicIfError(err)

	for rows.Next() {
		var id int
		var word string
		var documents hstore.Hstore
		//var documents []uint8

		err = rows.Scan(&id, &word, &documents)
		panicIfError(err)

		fmt.Printf("%v ", id)
		fmt.Printf("%v ", word)

		//postinglist := parseArray(documents)
		//fmt.Printf("%q\n", postinglist)

		var postinglist []*Posting
		for k, v := range documents.Map {
		//for k, v := range documents {
			article_id, err := strconv.Atoi(k)
			panicIfError(err)

			str := v.String
			var i, j int
			for i = 0; i < len(str); i++ {
				if str[i] == ':' {
					break
				}
			}
			for j = i + 1; j < len(str); j++ {
				if str[j] == ':' {
					break
				}
			}
			count, err := strconv.Atoi(str[i+1:j])
			panicIfError(err)

			var in_title bool
			if str[0] == '0' {
				in_title = false
			} else {
				in_title = true
			}

			posting := Posting{article_id: article_id, in_title: in_title, count: count}
			postinglist = append(postinglist, &posting)
		}

		println(len(postinglist))
	}
}
