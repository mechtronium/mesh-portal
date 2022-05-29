use mesh_portal::version::latest::cli::{RawCommand, Transfer};
use mesh_portal::version::latest::messaging::Response;


trait CliClient {

    fn send( &self, command_line: CommandLine ) -> Response;
//        let request = MsgRequest::new("CommandLine")?.with_body(line.into());

    fn line<C>( &self, content: C, transfers: Vec<Transfer> ) -> Response where C: ToString {
        let line = RawCommand{
            line: content.to_string(),
            transfers
        };
        send(line)
    }

}
