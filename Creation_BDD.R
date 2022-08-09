# Chargement du corpus, resultat du web scrapping et nettoyage des donnees
load('final.rda')

# Connexion a MySQL                                                                                                   
connection = dbConnect(RMySQL::MySQL(), host ="localhost", port = 8888, user = "root", password = "root")

# Suppression de la base si elle existe puis creation de la base de donnees
dbSendQuery(connection, "DROP DATABASE IF EXISTS employment;")
dbSendQuery(connection, "CREATE DATABASE employment;")

# Connexion à la base de donnees
mydb = dbConnect(RMySQL::MySQL(), host ="localhost", port = 8888, user = "root", password = "root", dbname="employment")


# Creation de la dimension localisation avec ses hierarchies
## Regions de France
region = read.csv('https://www.data.gouv.fr/fr/datasets/r/34fc7b52-ef11-4ab0-bc16-e1aae5c942e7', encoding = 'UTF-8')
colnames(region) = c("id_region","name_region")
dbWriteTable(mydb, "region", region, row.names=FALSE)
dbSendQuery(mydb, "ALTER TABLE region
MODIFY id_region VARCHAR(10) NOT NULL,
ADD PRIMARY KEY (id_region);")

dbSendQuery(mydb, "INSERT INTO region (id_region, name_region) VALUES ('5','Nouvelle-Caledonie');")


## Departements de France et cles etrangeres
departement = read.csv("https://www.data.gouv.fr/fr/datasets/r/70cef74f-70b1-495a-8500-c089229c0254", encoding = 'UTF-8')
departement = departement[,1:3]
colnames(departement) = c("id_department","name_dep","id_region")
dbWriteTable(mydb, "department", departement, row.names=FALSE)
dbSendQuery(mydb, "ALTER TABLE department
MODIFY id_department VARCHAR(15) NOT NULL,
MODIFY id_region VARCHAR(10),
ADD PRIMARY KEY (id_department),
ADD FOREIGN KEY (id_region) REFERENCES region(id_region);")

dbSendQuery(mydb, "INSERT INTO department (id_department, name_dep, id_region) VALUES ('99','Nouvelle-Caledonie','5');")

## Villes de France e partir des donnees et cles étrangeres
final[final$Dep=='NC',"Dep"] = '99' # remplacement pour la nouvelle caledonie
final = final[final$Ville!='Télétravail',]

final$id = as.numeric(factor(paste0(final$Ville, final$Dep)))
ville = distinct(as.data.frame(cbind(final$id, final$Ville, final$Dep)))

colnames(ville) = c("id_city","name_city","id_department")
dbWriteTable(mydb, "city", ville, row.names=FALSE)
dbSendQuery(mydb, "ALTER TABLE city
MODIFY id_city VARCHAR(20) NOT NULL,
MODIFY id_department VARCHAR(15),
ADD PRIMARY KEY (id_city),
ADD FOREIGN KEY (id_department) REFERENCES department(id_department);")

# Creation de la table de faits 
dbWriteTable(mydb, "job", final[,c("Company","Date","Titre","Content","Contract","Mean_salary","id")])

dbSendQuery(mydb, "ALTER TABLE job
CHANGE row_names id_job INT;")

dbSendQuery(mydb, "ALTER TABLE job
MODIFY id_job INT NOT NULL,
CHANGE id id_city VARCHAR(15),
ADD PRIMARY KEY (id_job),
ADD FOREIGN KEY (id_city) REFERENCES city(id_city)")

# Deconnexion de la base de donnees
dbDisconnect(mydb)
dbDisconnect(connection)



