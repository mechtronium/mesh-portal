pub mod parse;

use serde::*;

#[derive(Serialize,Deserialize)]
pub struct Host<T> {
    pub t: T
}

#[cfg(test)]
pub mod test {
    use crate::Host;
    use bincode;
    use anyhow::Error;



    #[test]
    pub fn test() -> Result<(),Error>{
        let host = Host { t: "hello".to_string() };
        let x = bincode::serialize(&host)?;

        bincode::deserialize(x.as_slice() )?;

        Ok(())

    }
}
