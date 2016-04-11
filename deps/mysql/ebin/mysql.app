{application,mysql,
             [{description,"This is mysql."},
              {vsn,"1.0a"},
              {modules,[db_agent,db_helper,db_mysql,db_mysqlutil,mysql,
                        mysql_auth,mysql_conn,mysql_recv,stat_db]},
              {registered,[]},
              {applications,[kernel,stdlib,sasl]},
              {start_phases,[]},
              {env,[]}]}.
