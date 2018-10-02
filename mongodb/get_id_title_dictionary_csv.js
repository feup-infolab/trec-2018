// time mongo --quiet wapo get_id_title_dictionary_csv.js | gzip > ../output/wapo-id_title_url-dictionary.csv.gz

print("id,title,url");

function quote(title) {
    if (title == null) return title;
    return '"' + title.replace(/"/g, '""') + '"';
}

db.articles
    .find()
    .forEach(function(doc) {
        print(quote(doc.id) + "," + quote(doc.title) + ',' + quote(doc.article_url));
    });

db.blog_posts
    .find()
    .forEach(function(doc) {
        print(quote(doc.id) + "," + quote(doc.title) + ',' + quote(doc.article_url));
    });