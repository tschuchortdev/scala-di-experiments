package example.di

case class MyConfig(host: String, xyz: String, declaredBy: String)
class Database(val declaredBy: String)
class KafkaClient(val declaredBy: String)
class HttpClient(val declaredBy: String)
class FooService(kafkaClient: KafkaClient, database: Database, val declaredBy: String)
class BarService(httpClient: HttpClient, database: Database, val declaredBy: String)