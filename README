
= erlang-ruby-marshal

== Main

Most comprehensive guide to Ruby marshal you can find here:

http://spec.ruby-doc.org/wiki/Marshaling

or read C source in Ruby sources tree:

http://www.ruby-doc.org/doxygen/1.8.4/marshal_8c-source.html

== Examples

Ruby:

cookie = {
  :user_id => 1024,
  :session_id => '56588901e819883d90fec315f4331bc2'
}

File.open('cookie.bin', 'w') do |f|
  f.write(Marshal.dump(cookie))
end

Erlang:

1> marshal:parse_file("cookie.bin").
[[{session_id,"56588901e819883d90fec315f4331bc2"},
  {user_id,1024}]]
