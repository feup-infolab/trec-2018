// time mongo --quiet wapo get_id_title_dictionary_csv.js > ../output/wapo-id_title_url-dictionary.csv

print("id,title,url");

function title_preprocess(title) {
    if (title == null) return title;
    return title.replace('"', '\\"');
}

db.articles
    .find()
    .forEach(function(doc) {
        print(doc.id + "," + '"' + title_preprocess(doc.title) + '",' + doc.article_url);
    });

db.blog_posts
    .find()
    .forEach(function(doc) {
        print(doc.id + "," + '"' + title_preprocess(doc.title) + '",' + doc.article_url);
    });